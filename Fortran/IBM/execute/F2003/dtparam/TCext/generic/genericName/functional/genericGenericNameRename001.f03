! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/genericName/functional/genericGenericNameRename001.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
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

   type :: base(k1)    ! (4)
      integer, kind            :: k1
      integer(k1), allocatable :: i
   end type

   interface baseconstr
      type(base(4)) function noarg()
         import base
      end function

      type(base(4)) function onearg(i)
         import base
         integer, intent(in) :: i
      end function
   end interface

   class(base(4)), allocatable :: b1

end module

module n
   use m, only: base, baseconstr, b1, hiddenbase => base

   type, extends(hiddenbase) :: child    ! (4)
      integer(k1) :: j
   end type

   interface childconstr
      module procedure noarg
      module procedure onearg
      module procedure twoarg
   end interface

   class(hiddenbase(4)), pointer :: b2
   class(child(4)), allocatable :: c1

   contains

      type(child(4)) function noarg()
         noarg%hiddenbase = baseconstr()
         noarg%j = -999
         print *, 'childnoarg'
      end function

      type(child(4)) function onearg(i)
         integer, intent(in) :: i
         onearg%hiddenbase = baseconstr(i)
         onearg%j = i
         print *, 'childonearg'
      end function

      type(child(4)) function twoarg(i,j)
         integer, intent(in) :: i, j
         twoarg%hiddenbase = baseconstr(i)
         twoarg%j = j
         print *, 'childtwoarg'
      end function

end module

type(base(4)) function noarg()
   use m, only: base
   allocate ( noarg%i, source = -999 )
   print *, 'basenoarg'
end function

type(base(4)) function onearg(i)
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