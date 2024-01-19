! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=base /tstdev/OO_tpbnd/specbnd/ftybn091t.f
! opt variations: -ql -qdefaultpv -qreuse=none

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : non_overridable
!*
!*  DESCRIPTION                : testing the parent procedures are
!*                               overridden, with multiple levels
!*                               overridden.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod
      integer :: int = 200
      character*20 :: c = "hi"

      type parent(k1)    ! (4)
         integer, kind :: k1
         integer(k1)   :: x
	 contains
      	 procedure, nopass, non_overridable :: bind => proc1
         procedure, nopass, non_overridable :: bind_r => proc2
      end type

      type, extends(parent) :: child    ! (4)
         type(parent(k1)) :: child
      contains
!* expect error message here: overridding isn't allowed with the non_overridable!* attribute in the parent type.
         procedure, nopass :: bind => proc1
      end type

!* expect error message here: Component name child of derived type definition thirgen, exists in parent type child.  A component in an extended type can not have the same name as any accessible component of its parent type.

      type, extends(child) :: thirGen    ! (4)
         type(child(k1)) :: y
      contains
         procedure, nopass :: bind => proc1
      end type

      type, extends(thirGen) :: fourGen    ! (4)
      contains
         procedure, nopass :: bind => proc1
      end type

      type, extends(fourGen) :: fifGen    ! (4)
      contains
         procedure, nopass :: bind => proc1
      end type

      contains
      subroutine proc1()
         int = 400
         c = "hi_again"
      end subroutine

      subroutine proc2()
         int = 0
         c = ""
      end subroutine

   end module

   end

