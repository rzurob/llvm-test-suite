! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound007.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

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
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : one argument with nopass (for generic-name tb)
!*                                  - with class hierarchy: one base type and two types extending it, and defined ambiguous tb in extended types
!*                                    but actually there is no ambiguity, since the types don't collide
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module genericName

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
   end type

   type, extends(base) :: childa    ! (20,4)
      integer(k1) :: j
      contains
         procedure, nopass :: printa
         generic :: print => printa
   end type

   type, extends(base) :: childb    ! (20,4)
      integer(k1) :: j
      contains
         procedure, nopass :: printa => printb
         generic :: print => printa
   end type

   contains

      subroutine printa(b)
         class(base(*,4)), intent(in) :: b

         print *, 'printa'

      end subroutine

      subroutine printb(b)
         class(base(*,4)), intent(in) :: b

         print *, 'printb'

      end subroutine

end module

program genericAmbiguityTypeBound007
   use genericName

   class(base(:,4)), allocatable :: b1

   allocate ( childa(20,4) :: b1 )

   select type ( b1 )
      type is ( childa(*,4) )
         call b1%print(b1)
   end select
   
   deallocate ( b1 )
   
   allocate ( childb(20,4) :: b1 )

   select type ( b1 )
      type is ( childb(*,4) )
         call b1%print(b1)
   end select

end program
