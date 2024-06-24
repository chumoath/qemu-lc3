lc3_ss = ss.source_set()
lc3_ss.add(files('boot.c'))
lc3_ss.add(when: 'CONFIG_LC3_MCU', if_true: files('lc3_mcu.c'))
lc3_ss.add(when: 'CONFIG_LC3_BOARD', if_true: files('lc3.c'))

hw_arch += {'lc3': lc3_ss}
