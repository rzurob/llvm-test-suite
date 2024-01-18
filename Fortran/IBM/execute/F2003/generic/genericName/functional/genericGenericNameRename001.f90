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
!*  DESCRIPTION                : generic-name: generic interface first defined as different name as derived type
!*                                             but later on renamed to be the same has derived type
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

   type :: base
      integer, allocatable :: i
   end type

   interface baseconstr
      type(base) function noarg()
         import base
      end function

      type(base) function onearg(i)
         import base
         integer, intent(in) :: i
      end function
   end interface

   class(base), allocatable :: b1

end module

module n
   use m, only: base, baseconstr, b1, hiddenbase => base

   type, extends(hiddenbase) :: child
      integer :: j
   end type

   interface childconstr
      module procedure noarg
      module procedure onearg
      module procedure twoarg
   end interface

   class(hiddenbase), pointer :: b2
   class(child), allocatable :: c1

   contains

      type(child) function noarg()
         noarg%hiddenbase = baseconstr()
         noarg%j = -999
         print *, 'childnoarg'
      end function

      type(child) function onearg(i)
         integer, intent(in) :: i
         onearg%hiddenbase = baseconstr(i)
         onearg%j = i
         print *, 'childonearg'
      end function

      type(child) function twoarg(i,j)
         integer, intent(in) :: i, j
         twoarg%hiddenbase = baseconstr(i)
         twoarg%j = j
         print *, 'childtwoarg'
      end function

end module

type(base) function noarg()
   use m, only: base
   allocate ( noarg%i, source = -999 )
   print *, 'basenoarg'
end function

type(base) function onearg(i)
   use m, only: base
   integer, intent(in) :: i
   allocate ( onearg%i, source = i )
   print *, 'baseonearg'
end function

program genericGenericNameRename001
   use m, only: baseconstr, base => baseconstr
   use n, only: childconstr, child => childconstr
   use n, only: b1, b2, c1

   allocate ( b1, source = base() )
   allocate ( b2, source = base(100) )
   allocate ( c1, source = child() )

   print *, b1%i
   print *, b2%i
   print *, c1%i, c1%j

   deallocate ( b1, b2, c1 )

   allocate ( b1, source = base(200) )
   allocate ( b2, source = child(300) )
   allocate ( c1, source = child(400,500) )

   print *, b1%i
   print *, b2%i
   print *, c1%i, c1%j

end program
