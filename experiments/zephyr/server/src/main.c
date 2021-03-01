#include <zephyr/types.h>
#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <sys/printk.h>
#include <sys/byteorder.h>
#include <zephyr.h>

#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#include <bluetooth/conn.h>
#include <bluetooth/uuid.h>
#include <bluetooth/gatt.h>

#include <sys/ring_buffer.h>
#include <usb/usb_device.h>
#include <drivers/uart.h>
#include <drivers/gpio.h>

/* Our own library of functions */
#include "defines.h"
#include "usb_cdc.h"

#define PRINT usb_printf
//#define PRINT printk


#define BT_UUID_MY_DEVICE              BT_UUID_DECLARE_16(0xffcc)
#define BT_UUID_MY_SERVICE             BT_UUID_DECLARE_16(0xff11)
#define BT_UUID_MY_CHARACTERISTIC      BT_UUID_DECLARE_16(0xff12)

volatile uint8_t discovered = 0;

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

/************/
/*  Server  */

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
  char text[len];

  if (offset + len > sizeof(value) + 1) {
    return BT_GATT_ERR(BT_ATT_ERR_INVALID_OFFSET);
  }

  memcpy(text + offset, buf, len);
  PRINT("got %s\n", text);
  return len;
}

/*
BT_GATT_CHARACTERISTIC = BT_GATT_ATTRIBUTE x 2


*/


BT_GATT_SERVICE_DEFINE(my_service,
		       BT_GATT_PRIMARY_SERVICE(BT_UUID_MY_SERVICE),
		       BT_GATT_CHARACTERISTIC(BT_UUID_MY_CHARACTERISTIC,
					      BT_GATT_CHRC_READ | BT_GATT_CHRC_WRITE,
					      BT_GATT_CHRC_AUTH | BT_GATT_PERM_READ |
					      BT_GATT_PERM_WRITE, read_value, write_value, &value),
		       );


/************/

/* THis field is used to configure service/characteristic discovery. */
static struct bt_gatt_discover_params discover_params;

struct remote_device* remote;

static uint8_t discover_temperature(struct bt_conn *conn,
				    const struct bt_gatt_attr *attr,
				    struct bt_gatt_discover_params *params) {
  int err;

  if (!attr) {
    if (!attr) PRINT("Discover_temperature: attr == NULL\r\n");
    
    PRINT("Discover complete\r\n");

    PRINT("Clearing params\r\n");
    (void)memset(params, 0, sizeof(struct bt_gatt_discover_params));

    return BT_GATT_ITER_STOP;
  }


  PRINT("Discover_temperature\r\n");
  

  PRINT("[ATTRIBUTE] handle %u\r\n", attr->handle);

  /* If we found the service */
  if (!bt_uuid_cmp(discover_params.uuid, remote->service)) {
    discover_params.uuid         = remote->characteristic;
    discover_params.start_handle = attr->handle + 1;
    discover_params.type         = BT_GATT_DISCOVER_CHARACTERISTIC;

    err = bt_gatt_discover(conn, &discover_params);
    if (err) {
      PRINT("Discover failed (err %d)\r\n", err);
    }
    /* If we found the characteristic */
  } else if (!bt_uuid_cmp(discover_params.uuid, remote->characteristic)) {
    set_handle(bt_gatt_attr_value_handle(attr), remote);
    discovered = 1;
    PRINT("Found characteristic handle\r\n");
  }

  return BT_GATT_ITER_STOP;
}

static const struct bt_data ad[] = {
  BT_DATA_BYTES(BT_DATA_FLAGS, (BT_LE_AD_GENERAL | BT_LE_AD_NO_BREDR)),
  // 0xcc & 0xff here is the device UUID
  BT_DATA_BYTES(BT_DATA_UUID16_ALL, 0xcc, 0xff, 0xaa, 0xff, 0x0a, 0x18),
};


static void connected(struct bt_conn *conn, uint8_t conn_err)
{
  char addr[BT_ADDR_LE_STR_LEN];

  bt_addr_le_to_str(bt_conn_get_dst(conn), addr, sizeof(addr));
	
  if (conn_err) {
    PRINT("Connection failed (err 0x%02x)\r\n", conn_err);
  } else {
    PRINT("Connected %s\r\n", addr);
    //    set_connection(conn, remote);
    remote->connection = bt_conn_ref(conn);


    // remote->address does not seem to be used anywhere.
    //con_addr = bt_conn_get_dst(conn);
    //remote->address = (const bt_addr_le_t *) *con_addr;
    //set_addr(*bt_conn_get_dst(conn), remote);

    discover_params.uuid         = remote->service;
    discover_params.func         = discover_temperature;
    discover_params.start_handle = 0x0001;
    discover_params.end_handle   = 0xffff;
    discover_params.type         = BT_GATT_DISCOVER_PRIMARY;
		
    uint8_t disc_err = bt_gatt_discover(remote->connection, &discover_params);
    if(disc_err) {
      PRINT("Discover failed(err %u)\r\n", disc_err);
      return;
    }
  }
}

static void disconnected(struct bt_conn *conn, uint8_t reason)
{
  PRINT("Disconnected (reason 0x%02x)\r\n", reason);

  if (remote->connection) {
    bt_conn_unref(remote->connection);
    remote->connection = NULL;
  }
}

static struct bt_conn_cb conn_callbacks = {
  .connected = connected,
  .disconnected = disconnected,
};

static void bt_ready(int e_val)
{
  if (e_val) {
    PRINT("Bluetooth init failed (err %d)\r\n", e_val);
    return;
  }
  
  int err;

  PRINT("Bluetooth initialized\r\n");

  err = bt_le_adv_start(BT_LE_ADV_CONN_NAME, ad, ARRAY_SIZE(ad), NULL, 0);
  if (err) {
    PRINT("Advertising failed to start (err %d)\n", err);
    return;
  }

  PRINT("Advertising successfully started\r\n");
}

void cb(struct bt_conn *conn, uint8_t err, struct bt_gatt_write_params *params) {
  printk("Performed write\n");
}

uint8_t device[]         = {0xaa, 0xff};
uint8_t service[]        = {0xa1, 0xff};
uint8_t characteristic[] = {0xa2, 0xff};

void main(void)
{
  int err;


  start_usb_cdc_thread();

  const struct device *d_led0;
  const struct device *d_led1;
  
  d_led0 = device_get_binding(LED_DEVICE_LABEL(led0));
  d_led1 = device_get_binding(LED_DEVICE_LABEL(led1));
  gpio_pin_configure(d_led0, LED_PIN(led0), GPIO_OUTPUT_ACTIVE | LED_FLAGS(led0));
  gpio_pin_configure(d_led1, LED_PIN(led1), GPIO_OUTPUT_ACTIVE | LED_FLAGS(led1));
  gpio_pin_set(d_led0, LED_PIN(led0), 0);
  gpio_pin_set(d_led1, LED_PIN(led1), 0);

  
  int led_test_state = 1;
  for (int i = 0; i < 5; i ++) {
    
    gpio_pin_set(d_led0, LED_PIN(led0), led_test_state);
    gpio_pin_set(d_led1, LED_PIN(led1), led_test_state);
    led_test_state = 1 - led_test_state;
    k_sleep(K_SECONDS(1));
  }

  gpio_pin_set(d_led0, LED_PIN(led0), 0);
  gpio_pin_set(d_led1, LED_PIN(led1), 0);

  
  k_sleep(K_SECONDS(4)); /* Give me a chance to connect the usb */   

  PRINT("Server starting up\r\n");
  
  /* Implement notification. At the moment there is no suitable way
   * of starting delayed work so we do it here
   */
  char* data = "server";

  err = bt_enable(bt_ready);
  if (err) {
    PRINT("Failed to init Bluetooth\r\n");
    return;
  }

  remote = new_remote_device(device, service, characteristic);
  if (!remote) {
    PRINT("Failed to new_remote_device\r\n");
  }
  PRINT("new_remote_device called successfully\r\n");
  
  set_message_payload(data, strlen(data) + 1, remote);
  remote->handle.func = cb;

  bt_conn_cb_register(&conn_callbacks);

  int led0_state = 1;
  while(!discovered) {
    gpio_pin_set(d_led0, LED_PIN(led0), led0_state);
    k_sleep(K_SECONDS(1));
    PRINT("have not discovered yet\r\n");

    led0_state = 1 - led0_state;
  }
  gpio_pin_set(d_led0, LED_PIN(led0), 0);

  int led1_state = 1; 
  while(1) {

    gpio_pin_set(d_led1, LED_PIN(led1), led1_state);
    
    k_sleep(K_SECONDS(1));

    int err = bt_gatt_write(remote->connection, &remote->handle);
    if(err) {
      PRINT("error while writing (err %d)\n", err);
    } else {
      PRINT("seems to have worked (err %d)\n", err);
    }

    led1_state = 1 - led1_state;
    
  }
}
