! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/F2003/generic/syntax/C460/genericC460Assignment002a.f
! opt variations: -qnol -qreuse=none

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Assignment( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : C460: specific-binding exist in parent type, generic assignment defined in gen3 type
!*                                     both base and child type should not call generic assignment
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)      i
      contains
         procedure, pass :: myassgn
   end type

   type, extends(base) :: emptychild    ! (20,4)
      contains
         procedure, pass :: myassgn => childassgn
   end type

   type, extends(emptychild) :: gen3    ! (20,4)
      integer(k1) j
      contains
         generic :: assignment( = ) => myassgn
   end type

   contains

      subroutine myassgn(a, b)
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in)  :: b
         a%i = b%i
         
         print *, 'base'
         
      end subroutine

      subroutine childassgn(a, b)
         class(emptychild(*,4)), intent(out) :: a
         class(base(*,4)), intent(in)  :: b
         a%i = b%i
         
         print *, 'child'
         
      end subroutine

end module

program genericC460Assignment002a
   use m
   
   type(base(20,4))       :: b1, b2
   type(emptychild(20,4)) :: c1, c2
   
   b1 = base(20,4)(10)
   b2 = base(20,4)(20)
   c1 = emptychild(20,4)(30)
   c2 = emptychild(20,4)(40)
   
   b1 = b2
   b2 = c1%base
   c1 = c2
   
   print *, b1, b2, c1, c2
   
end program
