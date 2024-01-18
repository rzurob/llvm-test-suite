! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/genericName/functional/genericGenericNameScalar006.f
! opt variations: -qnol -qnodeferredlp

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
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : generic-name: scalar derived type calling
!*                                             generic bindings with different numbers of arguments
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
      integer(k1)   :: i
      contains
         procedure, pass :: noarg
         procedure, pass :: onearg
         procedure, pass :: twoarg
         generic :: print => noarg, onearg, twoarg
   end type

   contains

      subroutine noarg (a)
         class(base(*,4)), intent(in) :: a

         print *,'noarg: ', a%i

      end subroutine

      subroutine onearg (a,b)
         class(base(*,4)), intent(in) :: a,b

         print *,'onearg: ', a%i, b%i

      end subroutine

      subroutine twoarg (a,b,c)
         class(base(*,4)), intent(in) :: a,b,c

         print *,'twoarg: ', a%i, b%i, c%i

      end subroutine

end module

program genericGenericNameScalar006
   use m

   type(base(20,4)) :: b1
   class(base(:,4)), allocatable :: b2

   allocate ( b2, source = base(20,4)(200) )
   b1 = base(20,4)(100)

   call b1%print()
   call b2%print()

   call b1%print(b2)
   call b2%print(b1)
   call b1%print(base(20,4)(50))

   call b1%print(b1, b2)
   call b2%print(b2, b1)
   call b1%print(b1, b1)
   
   call noarg(b1)
   call b2%onearg(b1)
   call b1%twoarg(b1, b1)

end program
