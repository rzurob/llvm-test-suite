! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specbnd/ftybn021a.f
! opt variations: -ql

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : overriding
!*
!*  DESCRIPTION                : Testing the nopass binding attribute,
!*                               the binding procedures wit one dummy
!*                               argument & overriding in the same
!*                               scope.
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


	module mod
		type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: x
		contains
      procedure, nopass :: bind => proc1
		end type base

   	type, extends(base) :: parent    ! (4)
 		contains
 		procedure, nopass :: bind => proc2
		end type

      type, extends(parent) :: child    ! (4)
      contains
      procedure, nopass :: bind => proc3
      end type

      contains
      integer function proc1(arg1)
			type(child(4)), intent(in) :: arg1
         proc1 = arg1%x
      end function proc1

      integer function proc2(arg1)
     	   type(child(4)), intent(in) :: arg1
         proc2 = arg1%x
      end function proc2

      integer function proc3(arg1)
         type(child(4)), intent(in) :: arg1
         proc3 = arg1%x
      end function proc3
	end module

   use mod

   type(child(4)) :: base_dt1
   type(child(4)) :: parent_dt1
   type(child(4)) :: child_dt1

   base_dt1%x = 10
   parent_dt1%x = 30
   child_dt1%x = 50

   if(base_dt1%bind(base_dt1) .ne. base_dt1%x)   error stop 2
   if(parent_dt1%bind(parent_dt1) .ne. parent_dt1%x) error stop 3
   if(child_dt1%bind(child_dt1) .ne. child_dt1%x) error stop 4

   end

