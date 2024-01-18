!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Abstractr Interface
!*
!*  PROGRAMMER                 : James Ren
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Functional test
!*
!*  DRIVER STANZA              : xlf90/95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : The abstract interface can be used for 
!*                               the procedures with BIND C attribute.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      use ISO_C_BINDING

      abstract interface
         subroutine sub(i) bind (c)
            import C_INT
            integer(C_INT) :: i
         end subroutine
      end interface

      procedure (sub) sum
      procedure (sub), pointer ::p
      p=>sum
      end program 
