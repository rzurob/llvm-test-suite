!*********************************************************************
!***********************************************************************

      !!! Subroutine name and module-variable's binding label conflict.

      subroutine s1 ()
        return
      end subroutine

      module modvar3
        bind(c, name='s1') :: j
      end module

      program p1
        use modvar3  ! make j and its binding label visible
        call s1  ! err:modvar binding label conflicts w/ subroutine name
      end program
