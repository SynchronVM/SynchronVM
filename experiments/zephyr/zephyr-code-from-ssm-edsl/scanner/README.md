# SSM and Zephyr test


- run the get_peng script to clone the ssm runtime



# BUGS
- the peng runtime system and zephyr do not agree on the definition
  of ULONG_MAX. under zephyr the (used to define NO_EVENT_SCHEDULED).
  Under zephyr this should be redefined to ULLONG_MAX

