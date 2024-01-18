! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : associate101kl
!*
!*  PROGRAMMER                 : David Forster (derived from associate101 by Robert Ma)
!*  DATE                       : 2007-06-29 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with associate construct (Input)
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
   type base (kb)
      integer, kind :: kb
      integer(kb) :: i
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program associate101kl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4)), allocatable :: b1
   type(base(4))               :: b3
   type(base(4)), allocatable  :: b4

   namelist /nml/ b1, b3, b4

   open (1, file = 'associate101kl.1', form='formatted', access='sequential' )
   allocate(b1, b4)

   b1%i = 0
   b3%i = 0
   b4%i = 0

   associate ( b1 => b3 , b3 => b4, b4 => b1 )

      read (1,NML=nml, iostat=stat, iomsg=msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4
      if ( b4%i /= 1234 ) error stop 2_4
      if ( b1%i /= 2345 ) error stop 3_4
      if ( b3%i /= 3456 ) error stop 4_4
   end associate

   if ( b1%i /= 1234 ) error stop 5_4
   if ( b3%i /= 2345 ) error stop 6_4
   if ( b4%i /= 3456 ) error stop 7_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 8_4
   if ( size(v_list, 1) /= 0 ) error stop 9_4

   read (unit, "(I4)", iostat=iostat )      dtv%i

   iomsg = 'dtioread'

end subroutine
