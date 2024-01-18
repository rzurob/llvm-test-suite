! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/OO_tpbnd/specbnd/ftybn021e.f
! opt variations: -ql -qreuse=none

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : accessibility testing with one module.
!*                               useing type-bound procedures to test
!*                               derived type components with multiple
!*                               access specifiers.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


      module mod
      integer, protected :: prot_a
      integer :: publ_a
      integer, private :: priv_a

      type base(k1)    ! (4)
      integer, kind        :: k1
      integer(k1)          :: prot_b, publ_b
      integer(k1), private :: priv_b
 	   contains
 	     procedure, nopass :: bind_base1 => setValue
 	     procedure, nopass :: bind_base2 => test_set
   	end type base

      type, extends(base) :: parent    ! (4)
      contains
 	     procedure, nopass :: bind_parent1 => prot_verify
 	     procedure, nopass :: bind_parent2 => priv_verify
      end type parent

      contains
      subroutine setValue(arg1, val)
      	type(base(4)), intent(inout) :: arg1
         integer, intent(in) :: val(3)
         arg1%prot_b = val(1)
         arg1%publ_b = val(2)
         arg1%priv_b = val(3)
      end subroutine

      subroutine test_set(arg1)
	      type(base(4)) :: arg1
   	   prot_a = arg1%prot_b
      	publ_a = arg1%publ_b
      	priv_a = arg1%priv_b
      end subroutine test_set

      logical function prot_verify(arg1)
 	     type(base(4)) :: arg1
   	      prot_verify =  (prot_a .eq. arg1%prot_b)
      end function prot_verify

      logical function priv_verify(arg1)
 	     type(base(4)) :: arg1
   	      priv_verify =  (priv_a .eq. arg1%priv_b)
      end function priv_verify
   end module

   use mod
   integer, dimension(3) :: val
   type(base(4)) :: base_dt
   type(parent(4)) :: parent_dt

   val = (/100, 200, 300/)
   call base_dt%bind_base1(base_dt, val)
   call base_dt%bind_base2(base_dt)

   if (prot_a .ne. 100 ) error stop 3
   if (publ_a .ne. 200 ) error stop 4

   if (parent_dt%bind_parent1(base_dt) .neqv. .true. )   error stop 6
   if (parent_dt%bind_parent2(base_dt) .neqv. .true. )   error stop 7
  end

