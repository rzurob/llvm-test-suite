! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/ftybn021b.f
! opt variations: -qnol

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : overriding
!*
!*  DESCRIPTION                : Testing the nopass binding attribute,
!*                               the binding procedures without dummy
!*                               argument & overriding in the same
!*                               scope.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


	module mod
		type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: x
		contains
      procedure, nopass :: bind => proc1
		end type base

   	type, extends(base) :: parent    ! (20,4)
 		contains
 		procedure, nopass :: bind => proc2
		end type

      type, extends(parent) :: child    ! (20,4)
      contains
      procedure, nopass :: bind => proc3
      end type

      contains
      integer function proc1()
         proc1 = 100
      end function proc1

      integer function proc2()
         proc2 = 200
      end function proc2

      integer function proc3()
         proc3 = 300
      end function proc3
	end module

   use mod

   type(base(20,4)) :: base_dt1
   type(parent(20,4)) :: parent_dt1
   type(child(20,4)) :: child_dt1

   base_dt1%x = 100
   parent_dt1%x = 200
   child_dt1%x = 300

   if(base_dt1%bind() .ne. base_dt1%x)   error stop 2
   if(parent_dt1%bind() .ne. parent_dt1%x) error stop 3
   if(child_dt1%bind() .ne. child_dt1%x) error stop 4

   end

