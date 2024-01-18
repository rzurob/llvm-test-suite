! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxopen-newunit08
!*
!*  PROGRAMMER                 :  (derived from associate001 by Robert Ma)
!*  DATE                       :
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008: NEWUNIT= specifier, Feature#:377344
!*  SECONDARY FUNCTIONS TESTED : DTIO( Derived Type Parameters),Namelist formatting
!*  REFERENCE                  :
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : Testing namelist fromatting in NEWUNIT=DTIO user-defind.
!*                               The "unit" argument in the user defined procedure
!*                               is the same as the NEWUNIT=value.
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

program fxopen_newunit08
   use m
   integer :: IVAR
   integer :: stat
   character(200) :: msg = ''
   class(base(4)), allocatable :: b1
   type(base(4))               :: b3
   type(base(4)), allocatable  :: b4

   namelist /nml/ b1, b3, b4

   open (NEWUNIT=IVAR, file = 'fxopen-newunit08.1', form='formatted', access='sequential' )
   allocate(b1, b4)

   b1%i = 2
   b3%i = 6
   b4%i = 8

   associate ( b1 => b3 , b3 => b4, b4 => b1 )

      write (IVAR,NML=nml, iostat=stat, iomsg=msg)

   end associate

   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

end program fxopen_newunit08


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base(4)), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( size(v_list, 1) /= 0 ) error stop 3_4

   write (unit, "(' i=',I4,1X)", iostat=iostat )      dtv%i

   iomsg = 'dtiowrite'

end subroutine
