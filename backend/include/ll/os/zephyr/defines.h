#ifndef DEFINES_H
#define DEFINES_H

#ifdef CONFIG_BT


#include <zephyr/types.h>
#include <stddef.h>
#include <errno.h>
#include <zephyr.h>
#include <sys/printk.h>

#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#include <bluetooth/conn.h>
#include <bluetooth/uuid.h>
#include <bluetooth/gatt.h>
#include <sys/byteorder.h>

struct remote_device {
	/* UUID of remote device */
	struct bt_uuid* uuid;

	/* UUID of remote devices service */
	struct bt_uuid* service;

	/* UUID of remote devices characteristic */
	struct bt_uuid* characteristic;

	/* Handle which to communicate via.
	   Contains an actual attribute handle. */
	struct bt_gatt_write_params handle;

	/* Address to remote device. */
	bt_addr_le_t address;

    /* The time of the remote device is approximately
	   my own time + delta_time. */
	signed long long delta_time;

	/* Connection object of remote device. */
	struct bt_conn* connection;
};

/*********** Construction & Destruction ***********/

/* Allocates memory for and partially initializes a remote_device struct.
   The fields related to connections are not valid until a connection has
   been established. */
struct remote_device* new_remote_device(uint8_t* uuid, uint8_t* service, uint8_t* characteristic);

/* Deallocate a remote device-object. */
void free_device(struct remote_device* device);


/*********** Setters ***********/

/* Set the handle used for communicating with a remote device. */
void set_handle(uint16_t handle, struct remote_device* device);

/* Set the message payload of a remote device, such that it can be transmitted to the remote device. */
void set_message_payload(const void* data, uint16_t len, struct remote_device* device);

void set_addr(bt_addr_le_t addr, struct remote_device* device);

/* Grab a new reference to the conn object and register it in device. The new object
   is released in free_device. */
void set_connection(struct bt_conn* conn, struct remote_device* device);


/*********** Connection management ***********/

/* Establish a connection to a remote device. Return 0 means error, otherwise success. */
int connect(bt_addr_le_t* addr, struct remote_device* device);


/*********** Communication ***********/

/* This registers a GATT server configured per the parameters found in device.
   Once this has been done remote devices can scan for this service and then
   communicate with device by writing to/reading from its attributes. */
int register_service(struct bt_uuid* service, struct bt_uuid* characteristic);

/* The inverse of the above method. */
int unregister_service(struct remote_device* device);


#endif
#endif
