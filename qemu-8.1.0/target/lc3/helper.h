DEF_HELPER_1(unsupported, noreturn, env)
DEF_HELPER_3(fullwr, void, env, i32, i32)
DEF_HELPER_2(fullrd, tl, env, i32)
DEF_HELPER_2(trap, void, env, i32)