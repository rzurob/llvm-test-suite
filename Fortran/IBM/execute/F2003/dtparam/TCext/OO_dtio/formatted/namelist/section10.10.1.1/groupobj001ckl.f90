!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : groupobj001ckl
!*
!*  PROGRAMMER                 : David Forster (derived from groupobj001c by Robert Ma)
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
!*                                        Input data being object components
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
   type base (kb) ! kb=4
      integer, kind :: kb
      integer(kb) :: i
   end type

   type container (kc) ! kc=4
      integer, kind :: kc
      type(base(kc)) :: b ! tcx: (kc)
   end type

end module

program groupobj001ckl
use m

   type(container(4)) :: b1 ! tcx: (4)
   character(200) :: msg = ''
   integer :: stat
   namelist /n1/ b1

   b1 = container(4) ( base(4) (-999) ) ! tcx: (4) ! tcx: (4)
   open (1, file='groupobj001ckl.1', form='formatted', access='sequential' )
   read (1, n1, iostat=stat,iomsg = msg)

   if ( ( stat /= 0 ) .or. ( msg /= '') ) error stop 1_4
   if ( b1%b%i /= 10 ) error stop 2_4

end



! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 2 changes
! type: container - added parameters (kc) to invoke with (4) / declare with (4) - 2 changes
