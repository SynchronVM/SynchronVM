

#ifndef _USB_CDC_H_
#define _USB_CDC_H_

extern void usb_printf(char *format, ...);

extern int usb_has_data(void);
extern int usb_get_char(void);
extern void usb_put_char(int i);
extern int usb_readl(char *buffer, int size);
extern void start_usb_cdc_thread(void);


#endif 
