! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/volatile/test/volatileC526Intent02.f
! with manual adjustment: must use same kind k1 in type base
! opt variations: -ql

!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 30/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : INTENT, VOLATILE
!*
!*  DESCRIPTION                : diagnostic TC for  C526
!*
!*   C526: if the volatile attribute is specified, the PARAMETER, EXTERNAL
!*         ,INTRINSIC OR INTENT(IN) shall not be specified.
!* ===================================================================

  module m
     contains
        subroutine sub(b)
          type base(k1)    ! (4)
             integer, kind :: k1
             sequence
             complex(k1)      x
          end type
          type(base(4)), intent(in):: b
          VOLATILE b
        end subroutine
  end module m

  program volatileC526Intent02
    use m

    type base(k1)    ! (4)
      integer, kind :: k1
      sequence
      complex(k1)      x
    end type
    type(base(4)) :: a
    a%x = (1.0,2.0)

    call sub(a)

  end program volatileC526Intent02

