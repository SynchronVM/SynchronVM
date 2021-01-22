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

#include "defines.h"

struct remote_device* new_remote_device( uint8_t* uuid
                                       , uint8_t* service
                                       , uint8_t* characteristic) {
    struct remote_device* device = (struct remote_device*) k_malloc(sizeof(struct remote_device));
    device->uuid           = (struct bt_uuid*) k_malloc(sizeof(struct bt_uuid_16));
    device->service        = (struct bt_uuid*) k_malloc(sizeof(struct bt_uuid_16));
    device->characteristic = (struct bt_uuid*) k_malloc(sizeof(struct bt_uuid_16));

    /* Hardcoded 2 for now, so they have to be 16 bits */
    bt_uuid_create(device->uuid          , uuid,           2);
    bt_uuid_create(device->service       , service,        2);
    bt_uuid_create(device->characteristic, characteristic, 2);

    /* 0 for now, not sure if we will need to do something smarter. */
    device->handle.offset = 0U;

    return device;
}

void free_device(struct remote_device* device) {
    /* Not sure what this does if the connection object is invalid,
       the documentation says nothing. */
    bt_conn_unref(device->connection);
    
    k_free(device->uuid);
    k_free(device->service);
    k_free(device->characteristic);
    k_free(device);
}

void set_handle(uint16_t handle, struct remote_device* device) {
    device->handle.handle = handle;
}

void set_message_payload(const void* data, uint16_t len, struct remote_device* device) {
    device->handle.data   = data;
    device->handle.length = len;
}

void set_addr(bt_addr_le_t addr, struct remote_device* device) {
    device->address = addr;
}

int connect(bt_addr_le_t* addr, struct remote_device* device) {
    return bt_conn_le_create( addr
                            , BT_CONN_LE_CREATE_CONN
                            , BT_LE_CONN_PARAM_DEFAULT
                            , &device->connection);
}

void set_connection(struct bt_conn* conn, struct remote_device* device) {
    device->connection = bt_conn_ref(conn   );
}

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
	printk("got %s\n", text);
	return len;
}
int register_service(struct bt_uuid* service, struct bt_uuid* characteristic) {
    /*
     * Allocate memory for and initialize the characteristic attribute.
     */
    struct bt_gatt_attr* chr_attrs = k_malloc(sizeof(struct bt_gatt_attr) * 3);
    if(chr_attrs == NULL) {
        printk("Allocating memory for attributes failed\n");
        return -1;
    }
    struct bt_gatt_attr* first      = chr_attrs;
    struct bt_gatt_attr* second     = chr_attrs+1;
    struct bt_gatt_attr* third      = chr_attrs+2;

    /* Service Attribute */
    struct bt_uuid* gatt_primary = k_malloc(sizeof(struct bt_uuid_16));
    uint8_t uuid_data[2] = { (uint8_t) BT_UUID_GATT_PRIMARY_VAL
                           , (uint8_t) (BT_UUID_GATT_PRIMARY_VAL >> 8)
                           };
    bt_uuid_create(gatt_primary, uuid_data, 2);

    first->uuid      = gatt_primary;
    first->perm      = BT_GATT_PERM_READ;
    first->read      = bt_gatt_attr_read_service;
    first->write     = NULL;
    first->user_data = service;
    first->handle    = 0;

    /* The first of the two characteristic attributes */
    struct bt_gatt_chrc* chrc = k_malloc(sizeof(struct bt_gatt_chrc));
    if(chrc == NULL) {
        printk("Allocating memory for chrc failed\n");
        return -1;
    }
    chrc->uuid         = characteristic;
    chrc->value_handle = 0U;
    chrc->properties   = BT_GATT_CHRC_READ | BT_GATT_CHRC_WRITE;

    struct bt_uuid* gatt_chrc = k_malloc(sizeof(struct bt_uuid_16));
    uuid_data[0] = (uint8_t) BT_UUID_GATT_CHRC_VAL;
    uuid_data[1] = (uint8_t) (BT_UUID_GATT_CHRC_VAL >> 8);
    bt_uuid_create(gatt_chrc, uuid_data, 2);

    second->uuid      = gatt_chrc;
    second->perm      = BT_GATT_PERM_READ;
    second->read      = bt_gatt_attr_read_chrc;
    second->write     = NULL;
    second->user_data = chrc;
    second->handle    = 0;

    /* The second characteristic attribute */
    third->uuid      = characteristic;
    third->perm      = BT_GATT_CHRC_AUTH | BT_GATT_PERM_READ | BT_GATT_PERM_WRITE;
    third->read      = read_value;
    third->write     = write_value;
    third->user_data = &value;
    third->handle    = 0;

	/*
	 * Allocate and create custom service
	 */
	struct bt_gatt_service* custom_svc = k_malloc(sizeof(struct bt_gatt_service));
    if(custom_svc == NULL) {
        printk("Could not allocate memory for custom_svc\n");
        return -1;
    }

    custom_svc->attrs      = chr_attrs;
    custom_svc->attr_count = 3;

    char str[BT_UUID_STR_LEN];
    bt_uuid_to_str(gatt_primary, str, BT_UUID_STR_LEN);

    printk("uuid was: %s\n", str);

    bt_uuid_to_str(gatt_chrc, str, BT_UUID_STR_LEN);

    printk("uuid was: %s\n", str);

	printk("Registering communication service\n");
	return bt_gatt_service_register(custom_svc);
}