! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/genericName/functional/genericGenericNameDeferred002.f
! opt variations: -qnock -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: generic tb with some specific and deferred binding and more class hierarchy
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

   type, abstract :: base(n1,k1)    ! (20,4)
      integer, kind        :: k1
      integer, len         :: n1
      integer(k1), pointer :: i
      contains
         procedure, pass  :: onearg
         procedure, pass  :: twoargs
         procedure, pass  :: twoargsint
         procedure(deferredinterfacereal), deferred, pass  :: twoargsreal
         procedure(deferredinterfacechar), deferred, pass  :: twoargschar

         generic :: myassociated => onearg, twoargs, twoargsint, twoargsreal

   end type

   abstract interface
      logical function deferredinterfacereal ( a, b)
         import base
         class(base(*,4)), intent(in) :: a
         real, pointer, intent(in) :: b
      end function
   end interface

   abstract interface
      logical function deferredinterfacechar ( a, b)
         import base
         class(base(*,4)), intent(in) :: a
         character(3), target, intent(in) :: b
      end function
   end interface

   contains

      logical function onearg(a)
         class(base(*,4)), intent(in) :: a

         onearg = associated (a%i)
         print *, 'onearg'

      end function

      logical function twoargs(a,b)
         class(base(*,4)), intent(in) :: a,b

         twoargs = associated (a%i,b%i)
         print *, 'twoargs'

      end function

      logical function twoargsint(a,b)
         class(base(*,4)), intent(in) :: a
         integer, pointer, intent(in) :: b

         twoargsint = associated (a%i,b)
         print *, 'twoargsint'

      end function

end module

module n
   use m

   type, extends(base), abstract :: child    ! (20,4)
      real(k1), pointer :: j
      contains
         procedure, pass :: twoargsreal
   end type

   contains

      logical function twoargsreal(a,b)
         class(child(*,4)), intent(in) :: a
         real, pointer, intent(in) :: b

         twoargsreal = associated (a%j,b)
         print *, 'twoargsreal'

      end function

end module

module o
   use n

   type, extends(child) :: gen3(k2,n2)    ! (20,4,1,3)
      integer, kind                      :: k2
      integer, len                       :: n2
      character(kind=k2,len=n2), pointer :: k
      contains
         procedure, pass :: twoargschar
         generic :: myassociated => twoargschar
   end type

   contains

      logical function twoargschar(a,b)
         class(gen3(*,4,1,*)), intent(in) :: a
         character(3), target, intent(in) :: b

         twoargschar = associated (a%k,b)
         print *, 'twoargschar'

      end function

end module

program genericGenericNameDeferred002
   use o

   class(base(:,4)), allocatable :: b1
   class(child(:,4)), pointer :: c1
   class(gen3(:,4,1,:)), allocatable :: g1

   integer, pointer :: i
   real, pointer :: r
   character(3), target :: c

   logical :: result

   allocate ( gen3(20,4,1,3) :: b1, c1, g1 )

   result = b1%myassociated()
   print *, result
   result = c1%myassociated()
   print *, result
   result = g1%myassociated()
   print *, result

   allocate ( i, source = 100_4 )
   b1%i => i

   result = b1%myassociated()
   print *, result
   result = c1%myassociated()
   print *, result
   result = g1%myassociated()
   print *, result

   result = b1%myassociated(i)
   print *, result
   result = c1%myassociated(i)
   print *, result
   result = g1%myassociated(i)
   print *, result

   c1%i => b1%i
   g1%i => c1%i

   result = b1%myassociated()
   print *, result
   result = c1%myassociated()
   print *, result
   result = g1%myassociated()
   print *, result

   result = b1%myassociated(i)
   print *, result
   result = c1%myassociated(i)
   print *, result
   result = g1%myassociated(i)
   print *, result

   result = b1%myassociated(c1)
   print *, result
   result = c1%myassociated(g1)
   print *, result
   result = g1%myassociated(b1)
   print *, result

   allocate ( r, source = 100.0_4  )

   c1%j => r

   result = c1%myassociated(r)
   print *, result
   result = g1%myassociated(r)
   print *, result

   result = b1%myassociated(c1)
   print *, result
   result = c1%myassociated(g1)
   print *, result
   result = g1%myassociated(b1)
   print *, result

   g1%j => r

   select type ( b1 )
      class is ( child(*,4) )
         b1%j=> g1%j
   end select

   result = g1%myassociated(r)
   print *, result

   result = b1%myassociated(r)
   print *, result

   c = 'ibm'
   g1%k => c

   result = g1%myassociated(c)
   print *, result

   select type ( c1 )
      class is ( gen3(*,4,1,*) )
         result = c1%myassociated(c)
         print *, result
   end select

   select type ( b1 )
      class is ( gen3(*,4,1,*) )
         result = b1%myassociated(c)
         print *, result
   end select

   select type ( c1 )
      class is ( gen3(*,4,1,*) )
         c1%k => g1%k
         result = c1%myassociated(c)
         print *, result
   end select

   select type ( b1 )
      class is ( gen3(*,4,1,*) )
         b1%k => g1%k
         result = b1%myassociated(c)
         print *, result
   end select

end program