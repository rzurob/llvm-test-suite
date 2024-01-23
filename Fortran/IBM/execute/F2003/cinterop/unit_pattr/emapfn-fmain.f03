      program emapfn  ! test linking via wcode EMAP on FunctioNs
        print '(A)', ''
        print '(A)', 'Fortran program'

        call f1f_via_f
        call f1c_via_f
        call f1f_via_c
        call f1c_via_c
      end
