      !!! Repeat this file with:
      !!! ----------------------------------------
      !!! illegal: missing function parentheses
      !!! ok: empty function parentheses
      !!! ok: 1 function argument
      !!! ok: 2 function arguments

      ! (by default, test with ARGS=`(a)')
#if ARG == 0                     /* nothing */
#  define ARGS
#elif ARG == 1                   /* empty parens */
#  define ARGS (   )
#elif ARG == 2 || !defined(ARG)  /* parens + 1 arg */
#  define ARGS (a  )
#elif ARG == 3                   /* parens + 2 args */
#  define ARGS (a,b)
#endif

      !! no result, no bind
      function f1 (a)    ! ok: no result, no bind(c)
         f1 = 3.0
      end function


      !! result keyword(s) only
      function f2 (a)   result(f0)  ! ok: one RESULT keyword
        f0 = 3.0;
      end

      function f3 (a)   result(f3)  ! illegal: result name same as function name
        f3 = 3.0;
      end

      function f4 (a)   result(fa) result(fa)  ! illegal: two same RESULT kwds
        fa = 3.0;
      end
      function f5 (a)   result(fa) result(fb)  ! illegal: two diff RESULT kwds
        fb = 3.0;
      end


      !! bind keyword(s) only
      function f6 (a)   bind(C)  ! ok: one BIND keyword
        f6 = 3.0;
      end

      function f7 (a)   bind(C) bind(C)  ! illegal: two BIND keywords
        f7 = 3.0;
      end


      !! bind-w-name keyword(s) only
      function f8 (a)   bind(C,name='f8B')  ! ok: one BIND keyword
        f8 = 3.0;
      end

      function f9 (a)   bind(C,name='f9B') bind(C,name='f9B')  ! illegal: two BIND keywords, same name
        f9 = 3.0;
      end
      function fa (a)   bind(C,name='faB') bind(C,name='faC')  ! illegal: two BIND keywords, diff name
        fa = 3.0;
      end


      !! result + bind
      function fb (a)   result(f0) bind(C)  ! ok: RESULT 1st, BIND 2nd
        f0 = 3.0;
      end
      function fc (a)   bind(C) result(f0)  ! ok: BIND 1st, RESULT 2nd
        f0 = 3.0;
      end

      !! result + bind-w-name
      function fd (a)   result(f0) bind(C,name='fdB')  ! ok: ..., BIND-NAME 2nd
        f0 = 3.0;
      end
      function fe (a)   bind(C,name='feB') result(f0)  ! ok: BIND-NAME 1st, ...
        f0 = 3.0;
      end


      !! multiple result + bind keyword(s)
      ! rrbb, bbrr, rbrb, brbr, rbrr, brbb.
      function ff (a)   result(f0) result(f0) bind(C) bind(C)  ! illegal*2: 4 keywords
        f0 = 3.0;
      end
      function fg (a)   bind(C) bind(C) result(f0) result(f0)  ! illegal*2: 4 keywords
        f0 = 3.0;
      end

      function fh (a)   result(f0) bind(C) result(f0) bind(C)  ! illegal*2: 4 keywords
        f0 = 3.0;
      end
      function fi (a)   bind(C) result(f0) bind(C) result(f0)  ! illegal*2: 4 keywords
        f0 = 3.0;
      end

      function fj (a)   result(f0) bind(C) result(f0) result(f0)  ! illegal*2: 4 keywords
        f0 = 3.0;
      end
      function fk (a)   bind(C) result(f0) bind(C) bind(C)  ! illegal*2: 4 kwds
        f0 = 3.0;
      end


      !! multiple result + bind-w-name keyword(s):
      ! rrbb,rrbB, bbrr,bBrr, rbrb,rbrB, brbr,brBr
      function fl (a)   result(f0) result(f0) bind(C,name='flB')        &
     & bind(C,name='flB')  ! illegal*2: 4 keywords
        f0 = 3.0;
      end
      function fm (a)   result(f0) result(f0) bind(C,name='fmB')        &
     & bind(C,name='fmC')  ! illegal*2: 4 keywords
        f0 = 3.0;
      end

      function fn (a)   bind(C,name='fnB') bind(C,name='fnB') result(f0)&
     & result(f0)  ! illegal*2: 4 keywords
        f0 = 3.0;
      end
      function fo (a)   bind(C,name='foB') bind(C,name='foC') result(f0)&
     & result(f0)  ! illegal*2: 4 keywords
        f0 = 3.0;
      end

      function fp (a)   result(f0) bind(C,name='fpB') result(f0)        &
     & bind(C,name='fpB')  ! illegal*2: 4 keywords
        f0 = 3.0;
      end
      function fq (a)   result(f0) bind(C,name='fqB') result(f0)        &
     & bind(C,name='fqC')  ! illegal*2: 4 keywords
        f0 = 3.0;
      end

      function fr (a)   bind(C,name='frB') result(f0) bind(C,name='frB')&
     & result(f0)  ! illegal*2: 4 keywords
        f0 = 3.0;
      end
      function fs (a)   bind(C,name='fsB') result(f0) bind(C,name='fsC')&
     & result(f0)  ! illegal*2: 4 keywords
        f0 = 3.0;
      end


      !! multiple result + bind-w-name keyword(s):
      ! rbrr,
      ! brbb:
      !   same-same-same
      !   diff1-diff1-diff2, diff1-diff2-diff2,
      !   diff1-diff2-diff3
      !
      function ft (a)   result(f0) bind(C,name='ftB') result(f0)        &
     & result(f0)  ! illegal*2: 4 keywords
        f0 = 3.0;
      end

      function fu (a)   bind(C,name='fuB') result(f0) bind(C,name='fuB')&
     & bind(C,name='fuB')  ! illegal*2: 4 keywords
        f0 = 3.0;
      end
      function fv (a)   bind(C,name='fvB') result(f0) bind(C,name='fvB')&
     & bind(C,name='fvC')  ! illegal*2: 4 keywords
        f0 = 3.0;
      end
      function fw (a)   bind(C,name='fwB') result(f0) bind(C,name='fwC')&
     & bind(C,name='fwC')  ! illegal*2: 4 keywords
        f0 = 3.0;
      end
      function fx (a)   bind(C,name='fxB') result(f0) bind(C,name='fxC')&
     & bind(C,name='fxD')  ! illegal*2: 4 keywords
        f0 = 3.0;
      end
