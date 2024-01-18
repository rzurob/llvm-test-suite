!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseOperator02
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-08-25
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : user-defined operators
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : user-defined operators
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Test operators; start with a single KIND.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUseModule

  implicit none

  type :: NoParms
  end type NoParms

  type, extends(NoParms) :: OneParm (k)
    integer, kind :: k
    integer(k) :: ifld = -1
  end type OneParm

  type, extends(OneParm) :: TwoParms (l)
    integer, len :: l
    character(l) :: chfld = 'nopqrstuvwxyz'
    integer(k) :: iarr(l) = -2
  end type TwoParms

  type, extends(TwoParms) :: ThreeParms (k2)
    integer, kind :: k2
    real(k2) :: rarr(l) = -1.3
  end type ThreeParms

  interface operator(+)
     module procedure noParmsPlus
     module procedure oneParmsPlus4
     module procedure twoParmsPlus4l
     module procedure threeParmsPlus4l4
  end interface


contains

  elemental function noParmsPlus(this, that)
    type(NoParms), intent(in) :: this
    class(*), intent(in) :: that
    type(NoParms) :: noParmsPlus
    noParmsPlus = this
  end function noParmsPlus


  elemental function oneParmsPlus4(this, that)
    type(OneParm(4)), intent(in) :: this
    class(*), intent(in) :: that
    type(OneParm(4)) :: oneParmsPlus4
    select type(that)
    class is (OneParm(4))
      oneParmsPlus4 % ifld = this % ifld + that % ifld
    class default
      oneParmsPlus4 = this
    end select
  end function oneParmsPlus4


  function twoParmsPlus4l(this, that)
    type(TwoParms(4,*)), intent(in) :: this
    class(*), intent(in) :: that
    type(TwoParms(4,this%l)) :: twoParmsPlus4l
    integer :: i
    select type(that)
    class is (TwoParms(4,*))
      if (this%l /= that%l) then
         twoParmsPlus4l = this
      else
         twoParmsPlus4l % OneParm = this%OneParm + that%OneParm
         do i = 1, this % l
            twoParmsPlus4l % chfld(i:i) = achar(iachar(this % chfld(i:i)) + iachar(that % chfld(i:i)))
         end do
         twoParmsPlus4l % iarr  = this % iarr + that % iarr
      end if
    class default
      twoParmsPlus4l = this
    end select
  end function twoParmsPlus4l


  function threeParmsPlus4l4(this, that)
    type(ThreeParms(4,*,4)), intent(in) :: this
    class(*), intent(in) :: that
    type(ThreeParms(4,this%l,4)) :: threeParmsPlus4l4
    select type(that)
    class is (ThreeParms(4,*,4))
      if (this%l /= that%l) then
         threeParmsPlus4l4 = this
      else
         threeParmsPlus4l4 % TwoParms = this%TwoParms + that%TwoParms
         threeParmsPlus4l4 % rarr = this % rarr + that % rarr
      end if
    class default
      threeParmsPlus4l4 = this
    end select
  end function threeParmsPlus4l4


end module dtpUseModule


program dtpUseOperator02
  use :: dtpUseModule
  implicit none
  type(OneParm(4))        :: p14va, p14vb, p14vc
  type(TwoParms(4,5))     :: p245va, p245vb, p245vc
  type(ThreeParms(4,5,4)) :: p3454va, p3454vb, p3454vc
  type(TwoParms(4,3))     :: p243va, p243vb, p243vc
  integer(4) :: i

  p14va = OneParm(4)(472088165_4)
  p14vb = OneParm(4)(7203_4)

  p245va = TwoParms(4,5)(1060045576_4,'!@#$%^',[(i**3,i=821,825)])
  p245vb = TwoParms(4,5)(16480_4,'!@#$%^',[(i,i=821,825)])

  p3454va = ThreeParms(4,5,4)(1060045576_4,'!@#$%^',[(i**3,i=821,825)], &
                              [4.1,5.3,9.1e10,-3.3,14.1e-5])
  p3454vb = ThreeParms(4,5,4)(1060045576_4,'!@#$%^',[(i**3,i=821,825)], &
                              [4.1,5.3,9.1e10,-3.31,14.1e-5])

  p243va = TwoParms(4,3)(1060045576_4,'!@#^',[(i**3,i=821,823)])
  p243vb = TwoParms(4,3)(1060045576_4,'#$%^',[(i**3,i=821,823)])

  print *, "p14va + a:", p14va + p14va
  print *, "p14vb + a:", p14vb + p14va
  print *, "p14va + b:", p14va + p14vb
  print *, "p14va + i:", p14va + 4
  print *, "p14va + l:", p14va + .true.
  print *, "p14va + c:", p14va + 'xx'
  print *, "p14va + p243va:", p14va + p243va
  print *, "p14va + p245va:", p14va + p245va
  print *, "p14va + p3454va:", p14va + p3454va
  p14vc = p14va + p14vb
  print *, "p14vc:", p14vc

  print *, "p245va + a:", p245va + p245va
  print *, "p245vb + a:", p245vb + p245va
  print *, "p245va + b:", p245va + p245vb
  print *, "p245va + i:", p245va + 4
  print *, "p245va + l:", p245va + .true.
  print *, "p245va + c:", p245va + 'xx'
  print *, "p245va + p14va:", p245va + p14va
  print *, "p245va + p243va:", p245va + p243va
  print *, "p245va + p3454va:", p245va + p3454va
  p245vc = p245va + p245vb
  print *, "p245vc:", p245vc

  print *, "p3454va + a:", p3454va + p3454va
  print *, "p3454vb + a:", p3454vb + p3454va
  print *, "p3454va + b:", p3454va + p3454vb
  print *, "p3454va + i:", p3454va + 4
  print *, "p3454va + l:", p3454va + .true.
  print *, "p3454va + c:", p3454va + 'xx'
  print *, "p3454va + p14va:", p3454va + p14va
  print *, "p3454va + p243va:", p3454va + p243va
  print *, "p3454va + p245va:", p3454va + p245va
  p3454vc = p3454va + p3454vb
  print *, "p3454vc:", p3454vc

  print *, "p243va + a:", p243va + p243va
  print *, "p243vb + a:", p243vb + p243va
  print *, "p243va + b:", p243va + p243vb
  print *, "p243va + i:", p243va + 4
  print *, "p243va + l:", p243va + .true.
  print *, "p243va + c:", p243va + 'xx'
  print *, "p243va + p14va:", p243va + p14va
  print *, "p243va + p245va:", p243va + p245va
  print *, "p243va + p3454va:", p243va + p3454va
  p243vc = p243va + p243vb
  print *, "p243vc:", p243vc

end program dtpUseOperator02
