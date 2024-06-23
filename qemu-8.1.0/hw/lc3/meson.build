lc3_ss = ss.source_set()
lc3_ss.add(files('boot.c'))
lc3_ss.add(when: 'CONFIG_LC3_MCU', if_true: files('atmega.c'))
lc3_ss.add(when: 'CONFIG_LC3_BOARD', if_true: files('arduino.c'))

hw_arch += {'lc3': lc3_ss}
