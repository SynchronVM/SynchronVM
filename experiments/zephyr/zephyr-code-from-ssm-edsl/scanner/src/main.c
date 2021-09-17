#include <zephyr/types.h>
#include <stddef.h>
#include <sys/printk.h>
#include <sys/util.h>

#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>

static uint8_t mfg_data[] = { 0xff, 0xff, 0x00 };


#define MAX_MESSAGES 100
#define MSG_ALIGNMENT 4

typedef struct {
  int8_t rssi;
  uint8_t adv_type;
  char str[256];
}scan_msg_t;


K_MSGQ_DEFINE(mq, sizeof(scan_msg_t), MAX_MESSAGES, MSG_ALIGNMENT);

int send_message(scan_msg_t msg) {

  return k_msgq_put(&mq,(void*)&msg, K_NO_WAIT);
}


int poll_message(scan_msg_t *msg) {

  int r = k_msgq_get(&mq, (void*)msg, K_NO_WAIT);

  if (r == -ENOMSG || r == -EAGAIN) {
    return 0;
  }
  return 1;
}

static const struct bt_data ad[] =
  {
   BT_DATA(BT_DATA_MANUFACTURER_DATA, mfg_data, 3),
  };

static void scan_cb(const bt_addr_le_t *addr, int8_t rssi, uint8_t adv_type,
		    struct net_buf_simple *buf){
  scan_msg_t msg;

  msg.rssi = rssi;
  msg.adv_type = adv_type;
  bt_addr_le_to_str (addr,msg.str,256);

  send_message(msg);
  
 
}

void main(void){
  struct bt_le_scan_param scan_param = {
					.type       = BT_HCI_LE_SCAN_PASSIVE,
					.options    = BT_LE_SCAN_OPT_NONE,
					.interval   = 0x0010,
					.window     = 0x0010,
  };
  int err;

  printk("Starting Scanner/Advertiser Demo\n");

  /* Initialize the Bluetooth Subsystem */
  err = bt_enable(NULL);
  if (err) {
    printk("Bluetooth init failed (err %d)\n", err);
    return;
  }

  /* printk("Bluetooth initialized\n"); */

  err = bt_le_scan_start(&scan_param, scan_cb);
  if (err) {
    printk("Starting scanning failed (err %d)\n", err);
    return;
  }

  do {
    k_sleep(K_MSEC(400));

    
    printk("looopie\r\n");

    
    scan_msg_t msg;
    
    while (poll_message(&msg) == 1) {
      printk("RSSI: %d\r\n", msg.rssi);
      printk("type: %u\r\n", msg.adv_type);
      printk("addr: %s\r\n", msg.str);
    }
    
       
    /* Start advertising */
    err = bt_le_adv_start(BT_LE_ADV_NCONN, ad, ARRAY_SIZE(ad),
    			  NULL, 0);
    if (err) {
      printk("Advertising failed to start (err %d)\n", err);
      return;
    }

    k_sleep(K_MSEC(400));

    err = bt_le_adv_stop();
    if (err) {
      printk("Advertising failed to stop (err %d)\n", err);
      return;
    }
  } while (1);
}
