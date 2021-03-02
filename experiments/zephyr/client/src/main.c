#include <zephyr/types.h>
#include <stddef.h>
#include <stdio.h>
#include <errno.h>
#include <zephyr.h>
#include <sys/printk.h>

#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#include <bluetooth/conn.h>
#include <bluetooth/uuid.h>
#include <bluetooth/gatt.h>
#include <sys/byteorder.h>
#include <sys/ring_buffer.h>

#include <drivers/gpio.h>
#include <drivers/uart.h>
#include <drivers/i2c.h>
#include <drivers/sensor.h>

/* Our own library of stuff! */ 
#include "defines.h"
#include "usb_cdc.h"
#include "ltr_303als.h"
#include "bme280.h"
//#include "uart.h"
#include "ll_uart.h"


struct remote_device* remote;
bool discovered = 0;

const struct bt_uuid * BT_UUID_MY_DEVICE           =   BT_UUID_DECLARE_16(0xffaa);
const struct bt_uuid * BT_UUID_MY_SERVICE          =   BT_UUID_DECLARE_16(0xffa1);
const struct bt_uuid * BT_UUID_MY_CHARACTERISTIC   =   BT_UUID_DECLARE_16(0xffa2);

#define PRINT usb_printf
//#define PRINT printk

/* LEDS */

#if DT_NODE_HAS_STATUS(DT_ALIAS(led0), okay)
#else
#error "NO LED0"
#endif
#if DT_NODE_HAS_STATUS(DT_ALIAS(led1), okay)
#else
#error "NO LED1"
#endif

#define LED_DEVICE_LABEL(X) DT_GPIO_LABEL(DT_ALIAS(X), gpios)
#define LED_PIN(X)          DT_GPIO_PIN(DT_ALIAS(X), gpios)
#define LED_FLAGS(X)        DT_GPIO_FLAGS(DT_ALIAS(X), gpios)


/****************************/
/*  Communication Protocol  */

/* All possible variants of a message that can be transmitted. */
typedef enum MessageType{ 
			 /* Request a remote devices time at the point of receiving this message. */
     REQUEST_TIME
    /* A reply to a `REQUEST_TIME` request. Contains the time in the `time` field. */
  , RESPOND_TIME
} message_type;

struct message {
  message_type type;
  union {
    /* Response to a `REQUEST_TIME` request */
    uint64_t time;
  };
};

/****************************/

/* *************** */
/*   UART buffers  */


uint8_t uart0_in_buffer[1024];
uint8_t uart0_out_buffer[1024];


/************/
/*  Server  */
/*
static char value[6];
static ssize_t read_value( struct bt_conn *conn
                         , const struct bt_gatt_attr *attr
						 , void *buf, uint16_t len, uint16_t offset) {
	const char *data = attr->user_data;
	return bt_gatt_attr_read(conn, attr, buf, len, offset, data, sizeof(value));
}

static ssize_t write_value( struct bt_conn *conn
                           , const struct bt_gatt_attr *attr
						   , const void *buf, uint16_t len, uint16_t offset
						   , uint8_t flags) {
	printk("received message!\n");
	//uint8_t *value = attr->user_data;
    char text[len];

	if (offset + len > sizeof(value) + 1) {
		return BT_GATT_ERR(BT_ATT_ERR_INVALID_OFFSET);
	}

	memcpy(text + offset, buf, len);
	printk("got %s\n", text);

	return len;
}
*/
/* In order to communicate one-to-one the devices need to set up a simple service where
   they agree to use a GATT attribute as a sort of handle. We can not say send(remote-device),
   but we can say send(remote-device,handle). */
/*
BT_GATT_SERVICE_DEFINE(my_service,
    BT_GATT_PRIMARY_SERVICE(BT_UUID_MY_SERVICE),
	    BT_GATT_CHARACTERISTIC(BT_UUID_MY_CHARACTERISTIC,
		                BT_GATT_CHRC_READ | BT_GATT_CHRC_WRITE,
						BT_GATT_CHRC_AUTH | BT_GATT_PERM_READ |
						BT_GATT_PERM_WRITE, read_value, write_value, &value),
);
*/
/************/


/* This field is used to configure service/characteristic discovery. */
static struct bt_gatt_discover_params discover_params;

static uint8_t discover_temperature(struct bt_conn *conn,
                                 const struct bt_gatt_attr *attr,
                                 struct bt_gatt_discover_params *params) {
	int err;

	if (!attr) {
		PRINT("Discover complete\r\n");
		(void)memset(params, 0, sizeof(*params));
		return BT_GATT_ITER_STOP;
	}

	PRINT("[ATTRIBUTE] handle %u\r\n", attr->handle);

    /* If we found the service */
	if (!bt_uuid_cmp(discover_params.uuid, remote->service)) {
		discover_params.uuid         = remote->characteristic;
		discover_params.start_handle = attr->handle + 1;
		discover_params.type         = BT_GATT_DISCOVER_CHARACTERISTIC;

		err = bt_gatt_discover(conn, &discover_params);
		if (err) {
			PRINT("Discover failed (err %d)\n", err);
		}
	/* If we found the characteristic */
	} else if (!bt_uuid_cmp(discover_params.uuid, remote->characteristic)) {
		set_handle(bt_gatt_attr_value_handle(attr), remote);
		discovered = 1;
		PRINT("Found characteristic handle\n");
	}

	return BT_GATT_ITER_STOP;
}

static bool eir_found(struct bt_data *data, void *user_data)
{
	bt_addr_le_t *addr = user_data;
	int i;

	char dev[BT_ADDR_LE_STR_LEN];

	bt_addr_le_to_str(addr, dev, sizeof(dev));

	//PRINT("[AD]: %u data_len %u, addr %s: \n", data->type, data->data_len, dev);

	switch (data->type) {
	case BT_DATA_UUID16_SOME:
	case BT_DATA_UUID16_ALL:
		if (data->data_len % sizeof(uint16_t) != 0U) {
			PRINT("AD malformed\n");
			return true;
		}

		for (i = 0; i < data->data_len; i += sizeof(uint16_t)) {
			struct bt_uuid *uuid;
			uint16_t u16;
			int err;

			memcpy(&u16, &data->data[i], sizeof(u16));
			uuid = BT_UUID_DECLARE_16(sys_le16_to_cpu(u16));
			// compare against the remote device uuid
			if (bt_uuid_cmp(uuid, remote->uuid)) {
				continue;
			} else {
				PRINT("Found device uuid\n");
			}

			err = bt_le_scan_stop();
			if (err) {
				PRINT("Stop LE scan failed (err %d)\n", err);
				continue;
			}

			err = connect(addr, remote);

			if (err) {
				PRINT("Create conn failed (err %d)\n", err);
			}
			set_addr(*addr, remote);

			return false;
		}
	}

	return true;
}

static void device_found(const bt_addr_le_t *addr, int8_t rssi, uint8_t type,
			 struct net_buf_simple *ad)
{
	char dev[BT_ADDR_LE_STR_LEN];

	bt_addr_le_to_str(addr, dev, sizeof(dev));

	/* We're only interested in connectable events */
	if (type == BT_GAP_ADV_TYPE_ADV_IND ||
	    type == BT_GAP_ADV_TYPE_ADV_DIRECT_IND) {
		bt_data_parse(ad, eir_found, (void *)addr);
	}
}

static void start_scan(void)
{
	int err;

	/* Use active scanning and disable duplicate filtering to handle any
	 * devices that might update their advertising data at runtime. */
	struct bt_le_scan_param scan_param = {
		.type       = BT_LE_SCAN_TYPE_ACTIVE,
		.options    = BT_LE_SCAN_OPT_NONE,
		.interval   = BT_GAP_SCAN_FAST_INTERVAL,
		.window     = BT_GAP_SCAN_FAST_WINDOW,
	};

	err = bt_le_scan_start(&scan_param, device_found);
	if (err) {
		PRINT("Scanning failed to start (err %d)\n", err);
		return;
	}

	PRINT("Scanning successfully started\n");
}

uint8_t isConnected = 0;
static void connected(struct bt_conn *conn, uint8_t conn_err)
{
	char addr[BT_ADDR_LE_STR_LEN];
	int err;
	
	bt_addr_le_to_str(bt_conn_get_dst(conn), addr, sizeof(addr));
	isConnected = 1;

	if (conn_err) {
		PRINT("Failed to connect to %s (%u)\n", addr, conn_err);

		bt_conn_unref(remote->connection);
		remote->connection = NULL;

		start_scan();
		return;
	}

	PRINT("Connected: %s\n", addr);

	discover_params.uuid         = remote->service;
	discover_params.func         = discover_temperature;
	discover_params.start_handle = 0x0001;
	discover_params.end_handle   = 0xffff;
	discover_params.type         = BT_GATT_DISCOVER_PRIMARY;
	
	err = bt_gatt_discover(remote->connection, &discover_params);
	if(err) {
		PRINT("Discover failed(err %d)\n", err);
		return;
	}
}

static void disconnected(struct bt_conn *conn, uint8_t reason)
{
	char addr[BT_ADDR_LE_STR_LEN];

	bt_addr_le_to_str(bt_conn_get_dst(conn), addr, sizeof(addr));

	PRINT("Disconnected: %s (reason 0x%02x)\n", addr, reason);
	isConnected = 0;

	if (remote->connection != conn) {
		return;
	}

	bt_conn_unref(remote->connection);
	remote->connection = NULL;

	start_scan();
}

static struct bt_conn_cb conn_callbacks = {
	.connected    = connected,
	.disconnected = disconnected,
};

void start_bt(void)
{
  int err;
  err = bt_enable(NULL);

  if (err) {
    PRINT("Bluetooth init failed (err %d)\r\n", err);
    return;
  }

  PRINT("Bluetooth initialized\n");
  err = register_service(BT_UUID_MY_SERVICE, BT_UUID_MY_CHARACTERISTIC);
  if(err) {
    PRINT("Registering service failed (err %d)\n",err);
    return;
  } else {
    PRINT("It seemed that registering went fine\n");
  }


  bt_conn_cb_register(&conn_callbacks);

  start_scan();
}

void cb(struct bt_conn *conn, uint8_t err, struct bt_gatt_write_params *params) {
    PRINT("Performed write (err %d) (offset %d) (len %d)\n", err, params->offset, params->length);
}

/* Little endian rep. of remote device's uuid */
uint8_t device[]         = {0xcc, 0xff};
uint8_t service[]        = {0x11, 0xff};
uint8_t characteristic[] = {0x12, 0xff};

char* data = "client";

void main(void) {

  /* Start USB_CDC and set up LEDs  
     LEDS depend on definitions in the devicetree (dts file)
   */
  	
  start_usb_cdc_thread();
    
  const struct device *d_led0;
  const struct device *d_led1;
  
  d_led0 = device_get_binding(LED_DEVICE_LABEL(led0));
  d_led1 = device_get_binding(LED_DEVICE_LABEL(led1));
  gpio_pin_configure(d_led0, LED_PIN(led0), GPIO_OUTPUT_ACTIVE | LED_FLAGS(led0));
  gpio_pin_configure(d_led1, LED_PIN(led1), GPIO_OUTPUT_ACTIVE | LED_FLAGS(led1));
  gpio_pin_set(d_led0, LED_PIN(led0), 0);
  gpio_pin_set(d_led1, LED_PIN(led1), 0);

  
  k_sleep(K_SECONDS(5));
  PRINT("Starting up\r\n");

  /* configure ltr-303als */
  if (!als_init()) {
    PRINT("ALS: Unable to initialize\r\n");
  }

  if (!als_set_gain(1)) {
    PRINT("ALS: Unable to set gain\r\n");
  }
  
  uint8_t data[16];

  int counter  = 0; 
  while (counter < 10) {

    uint16_t ch0 = 0;
    uint16_t ch1 = 0;
      
    if (als_read_data(&ch0,&ch1)) {
      PRINT("ALS CH0: %u\r\n", ch0);
      PRINT("ALS CH1: %u\r\n", ch1);	
    } else {
      PRINT("ALS: Error reading data register");
    }
    k_sleep(K_SECONDS(1));
    counter ++;
  }
  
  /* BME280 */


  if (!bme_init()) { 
    PRINT("BME280: Device not found\r\n");
    return;
  } else {
    PRINT("BME280: OK!\r\n");
  }

  while (counter < 20) {
    int32_t i, d;
    if (bme_sample()) {
      PRINT("--------------------------------\r\n");
      bme_get_temperature(&i, &d);
      PRINT("Temperature: %d.%06d\r\n", i, d);
      bme_get_pressure(&i,&d);
      PRINT("Pressure: %d.%06d\r\n", i, d);
      bme_get_humidity(&i, &d);
      PRINT("Humidity: %d.%06d\r\n", i, d);
    }
    k_sleep(K_SECONDS(1));
    counter++;
  }
  
  /* configure uart */

  ll_driver_t uart_drv;
  uart_dev_t uart0;
  
  if (ll_uart_init(&uart_drv, UART0, &uart0, uart0_in_buffer, 1024, uart0_out_buffer, 1024)) {
    PRINT("LL_UART: OK!\r\n");
  } else {
    PRINT("LL_UART: Failed!\r\n");
  }

  const char *hello = "hello world\r\n";
  
  while (counter < 30) {
    //uart_printf(&uart0, "Hello World\r\n");
    ll_write(&uart_drv, hello, strlen(hello));
    k_sleep(K_SECONDS(1));
    counter++;
  }

  /* BT Create and initialise remote device information */
  remote = new_remote_device(device, service, characteristic);
  set_message_payload(data, strlen(data) + 1, remote);
  remote->handle.func   = cb;

  start_bt();

  int led0_state = 1;
  
  while(!discovered) {
    gpio_pin_set(d_led0, LED_PIN(led0), led0_state);
    led0_state = 1 - led0_state;
    k_sleep(K_SECONDS(1));
    
    int c;

    while ((c = uart_get_char(&uart0)) != -1 ) { 
       PRINT("%c", (char)c);
    }

    
    //PRINT("have not discovered yet\n");
    PRINT("CLIENT: Have not discovered yet\r\n");
    uart_printf(&uart0,"CLIENT: Have not discovered yet\r\n");
  }
  int led1_state = 1;
  while(1) {
    gpio_pin_set(d_led1, LED_PIN(led1), led1_state);
    led1_state = 1 - led1_state;
    k_sleep(K_SECONDS(1));
    
    int err = bt_gatt_write(remote->connection, &remote->handle);
    if(err) {
      //PRINT("error while writing (err %d)\n", err);
      usb_printf("CLIENT: error while writing (err %d)\r\n", err);
      uart_printf(&uart0, "CLIENT: error while writing (err %d)\r\n", err);
    }
    
  }
}
