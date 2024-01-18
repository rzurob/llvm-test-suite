! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specbnd/ftybn022e.f
! opt variations: -ql -qreuse=self

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftybn022e.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn022e.f
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


	module mod1
      integer, protected :: prot_a
      integer :: publ_a
      integer, private :: priv_a

      type base(k1,k2)    ! (4,4)
      integer, kind        :: k1,k2
      integer(k1)          :: prot_b, publ_b
      integer(k2), private :: priv_b
 	   contains
 	     procedure, nopass :: bind_base1 => setValue
 	     procedure, nopass :: bind_base2 => test_set
 	     procedure, nopass :: bind_base3 => priv_verify
   	end type base

      contains
      subroutine setValue(arg1, val)
      	type(base(4,4)), intent(inout) :: arg1
         integer, intent(in) :: val(3)
         arg1%prot_b = val(1)
         arg1%publ_b = val(2)
         arg1%priv_b = val(3)
      end subroutine

      subroutine test_set(arg1)
	      type(base(4,4)) :: arg1
   	   prot_a = arg1%prot_b
      	publ_a = arg1%publ_b
      	priv_a = arg1%priv_b
      end subroutine test_set

      logical function priv_verify(arg1)
 	     type(base(4,4)) :: arg1
   	      priv_verify =  (priv_a .eq. arg1%priv_b)
      end function priv_verify
   end module

   module mod2
   use mod1
      type, extends(base) :: parent    ! (4,4)
      contains
 	     procedure, nopass :: bind_parent => prot_verify
      end type parent

   contains
      logical function prot_verify(arg1)
        type(base(4,4)) :: arg1
            prot_verify =  (prot_a .eq. arg1%prot_b)
      end function prot_verify
   end module

   use mod2
   integer, dimension(3) :: val
   type(base(4,4)) :: base_dt
   type(parent(4,4)) :: parent_dt

   val = (/100, 200, 300/)
   call base_dt%bind_base1(base_dt, val)
   call base_dt%bind_base2(base_dt)

   if (prot_a .ne. 100 ) error stop 3
   if (publ_a .ne. 200 ) error stop 4

   if (base_dt%bind_base3(base_dt) .neqv. .true. )   error stop 7
   if (parent_dt%bind_parent(base_dt) .neqv. .true. )   error stop 6
  end

