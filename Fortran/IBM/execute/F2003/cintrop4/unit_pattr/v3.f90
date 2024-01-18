!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: $TR_SRC/vfloop.sh -cf $TR_SRC/c-f-pair.sh
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************

  !!! Interoperable Fortran subroutine referenced from Fortran as well as C.

  SUBROUTINE s1 () BIND(C, NAME="s2")
    print*, 'hello'
  END SUBROUTINE s1

  PROGRAM prog1
    INTERFACE
      SUBROUTINE s1 () BIND(C, NAME='s2')  ! Could be a C fn `void s1 (void)'
      END SUBROUTINE s1                    !   but actually implemented in Ftn.
      SUBROUTINE s3 () BIND(C)
      END SUBROUTINE s3
    END INTERFACE

    CALL s1  ! Fortran BIND(C) subroutine called through its Fortran name
    CALL s3  ! Fortran calls C calls Fortran
  END PROGRAM

  ! Expected result:
  !  hello
  !  hello
