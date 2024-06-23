gen = [
  decodetree.process('insn.decode', extra_args: [ '--decode', 'decode_insn',
                                                  '--insnwidth', '16' ])
]

lc3_ss = ss.source_set()
lc3_system_ss = ss.source_set()

lc3_ss.add(gen)
lc3_ss.add(files(
  'translate.c',
  'helper.c',
  'cpu.c',
  'gdbstub.c',
  'disas.c'))

lc3_system_ss.add(files('machine.c'))

target_arch += {'lc3': lc3_ss}
target_softmmu_arch += {'lc3': lc3_system_ss}
