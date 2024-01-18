! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/C611/proceRef003.f
!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp proceRef003.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 09/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        non-polymorphic non-rightmost array object with elemental type-bound
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

   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   contains
      procedure, pass :: getid => getbaseid
   end type

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      integer(k2)   :: rid
   contains
      procedure, pass :: getid => getchildid
   end type

   
contains

   elemental integer function getbaseid(a)
      class(base(4)), intent(in) :: a
      getbaseid = a%id
   end function

   elemental integer function getchildid(a)
      class(child(4,4)), intent(in) :: a
      getchildid = a%rid
   end function

end module

program proceRef003
   use m

   type(child(4,4)), dimension(2) :: c1 = (/ child(4,4) (1,2),child(4,4) (4,5) /)
   class(child(4,4)), allocatable, dimension(:) :: c2
   type(child(4,4)), pointer, dimension(:,:) :: c3

   allocate(  c2(2), source = (/ child(4,4) (1,3),child(4,4) (4,6) /) )
   allocate ( c3(2,2), source = reshape (source = (/child(4,4) (1,2),child(4,4) (4,5), c2 /), shape=(/2,2/) ) )

   print *,c1%getid()
   print *,c1%base%getid()

   print *,c2%getid()
   print *,c2%base%getid()

   print *,c3%getid()
   print *,c3%base%getid()

end program
