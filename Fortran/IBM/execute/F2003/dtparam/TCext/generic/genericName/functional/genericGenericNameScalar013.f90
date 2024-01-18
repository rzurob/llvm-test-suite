! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/genericName/functional/genericGenericNameScalar013.f
! opt variations: -ql

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
!*                                             multiple (public, private) generic binding pointing to same and different spec bindings
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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure, private, nopass :: noarg
         procedure, private, nopass :: onearg
         procedure, private, pass :: twoargs
         generic, private :: a => noarg, onearg, twoargs
         generic :: b => noarg, onearg, twoargs
         generic, private :: c => noarg, onearg
         generic :: d => noarg, twoargs

   end type

   contains

      subroutine noarg()
         print *,'noarg'
      end subroutine

      subroutine onearg(a)
         integer, intent(in) :: a
         print *,'onearg:', a
      end subroutine

      subroutine twoargs(a,b)
         class(base(4)), intent(in) :: a, b
         print *,'twoargs:', a%i, b%i
      end subroutine

      subroutine callprivatebindingA(a)
         class(base(4)), intent(in) :: a

         call a%a()
         call a%a(10)
         call a%a(a)

      end subroutine
      
      subroutine callprivatebindingC(a)
         class(base(4)), intent(in) :: a

         call a%c()
         call a%c(200)

      end subroutine
      
      subroutine callprivatespecbinding(a)
         class(base(4)), intent(in) :: a

         call a%noarg()
         call a%onearg(a%i)
         call a%twoargs(a)

      end subroutine

end module


program genericGenericNameScalar013
   use m

   type(base(4)) :: b1

   b1%i = 50
   call callprivatebindingA(b1)

   b1%i = 150
   call b1%b()
   call b1%b(100)
   call b1%b(b1)

   call callprivatebindingC(b1)

   call b1%d()
   call b1%d(base(4)(300))
   
   call callprivatespecbinding(base(4)(500))

end program
