! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : input104kl
!*
!*  PROGRAMMER                 : David Forster (derived from input104 by Robert Ma)
!*  DATE                       : 2007-07-20 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.1 Namelist Input Values
!*                                        Derived type variable shall be expanded into intrinsic types
!*                                       (no dtio procedure involved, with array components)
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
   type base (kb,lb1,lb2) ! kb,lb=4,3
      integer, kind :: kb
      integer, len :: lb1,lb2
      integer(kb)   :: i(lb1) = 9 ! (/ 9, 9, 9 /)
      real(kb)      :: r(lb1) = 9.0 ! (/ 9.0, 9.0, 9.0 /)
      character(lb2) :: c(lb1) = 'xxx' ! (/ 'xxx', 'xxx', 'xxx' /)
   end type
end module

program input104kl
   use m

   integer :: stat
   character(150) :: msg = ''
   procedure(logical) :: precision_r4
   type(base(4,3,3))               :: b1 ! tcx: (4,3,3)
   type(base(4,:,:)), pointer      :: b2 ! tcx: (4,:,:)
   type(base(4,:,:)), allocatable  :: b3 ! tcx: (4,:,:)

   namelist /n1/ b1, b2
   namelist /n1/ b3

   allocate(base(4,3,3):: b2,b3) ! tcx: base(4,3,3)
   open (1, file='input104kl.1', form='formatted', access='sequential', blank='zero' )

   read (1, n1, iostat = stat, iomsg = msg)
   if ( ( stat /= 0 ) .or. ( msg /= '' ) ) error stop 1_4
   
   print *, b1
   print *, b2
   print *, b3

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb1,lb2) to invoke with (4,3,3) / declare with (4,*,*) - 3 changes
