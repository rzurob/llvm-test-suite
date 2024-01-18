! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : associate002akl
!*
!*  PROGRAMMER                 : David Forster (derived from associate002a by Robert Ma)
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
!*                                        Try namelist formatting with multiple levels associate construct (Output)
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

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program associate002akl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4)), allocatable :: b1
   class(base(4)), pointer     :: b2
   type(base(4))               :: b3
   type(base(4)), allocatable  :: b4
   type(base(4)), pointer      :: b5

   namelist /nml/ b1, b2, b3, b4, b5

   open (1, file = 'associate002akl.1', form='formatted', access='sequential' )
   allocate(b1,b2,b4,b5)

   b1%i = 2
   b2%i = 4
   b3%i = 6
   b4%i = 8
   b5%i = 10

   associate ( b11 => b1 , b13 => b3, b14 => b4 )

      b11%i = 4
      b13%i = 12
      b14%i = 16

      write (1,NML=nml, iostat=stat, iomsg=msg)
      if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

      associate ( b12 => b2 , b15 => b5 )
         b11%i = 4-1
         b13%i = 12-1
         b14%i = 16-1
         b12%i = 8
         b15%i = 20

         write (1,NML=nml, iostat=stat, iomsg=msg)
         if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

         associate ( b111 => b11, b112 => b12, b113 => b13, b114 => b14, b115 => b15 )
            b111%i = 2
            b112%i = 4
            b113%i = 6
            b114%i = 8
            b115%i = 10

            write (1,NML=nml, iostat=stat, iomsg=msg)
            if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

         end associate
      end associate
   end associate

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   write (unit, "(' i=',I4,1X)", iostat=iostat )      dtv%i

   iomsg = 'dtiowrite'

end subroutine
