! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : groupobj001akl
!*
!*  PROGRAMMER                 : David Forster (derived from groupobj001a by Robert Ma)
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
!*  DESCRIPTION                : Testing: Section 10.10.1.1 Namelist group object names
!*                                        Input data being object components of sequential type entities
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

   type base (kb,lb) ! kb,lb=4,4
      integer, kind :: kb
      integer, len :: lb
      sequence
      character(kb) :: c = 'xxxx'
      integer(kb)   :: i = 999
      real(kb)      :: r = -9.0
   end type

end module

program groupobj001akl
   use m
   implicit type(base(4,4)) (x-z) ! tcx: (4,4)
   integer :: stat
   character(200) :: msg
   type(base(4,:)), allocatable :: b1 ! tcx: (4,:)
   type(base(4,:)), pointer     :: b2 ! tcx: (4,:)
   type(base(4,4))              :: b3 ! tcx: (4,4)
   procedure(logical) :: precision_r4
   namelist /nml/ b1, b2, b3, x1

   allocate ( base(4,4):: b1 , b2 ) ! tcx: base(4,4)

   open (1, file='groupobj001akl.1', form='formatted', access='sequential' )

   read (1, nml, iostat = stat, iomsg = msg)

   if ( ( b1%i /= 999  ) .or. ( b1%c /= 'abcd' ) .or. ( .not. precision_r4( b1%r, -9.0 ) ) ) error stop 1_4
   if ( ( b2%i /= 2002 ) .or. ( b2%c /= 'efgh' ) .or. ( .not. precision_r4( b2%r, 1.0  ) ) ) error stop 2_4
   if ( ( b3%i /= 3003 ) .or. ( b3%c /= 'ijkl' ) .or. ( .not. precision_r4( b3%r, -9.0 ) ) ) error stop 3_4
   if ( ( x1%i /= 4004 ) .or. ( x1%c /= ''     ) .or. ( .not. precision_r4( x1%r, 2.0  ) ) ) error stop 4_4

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,4) / declare with (4,*) - 4 changes
