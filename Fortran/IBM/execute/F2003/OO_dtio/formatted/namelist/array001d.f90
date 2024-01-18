!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: array001d.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic/nonpoly
!*                                        array with unlimited polymorphic component (Output)
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

   type :: data
      integer :: i
   end type

   type :: base
      class(*), allocatable :: u
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
  end interface

end module

program array001d
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1(:)
   class(base), pointer     :: b2(:,:)
   type(base)               :: b3(2,2)
   class(base), pointer     :: b4(:,:)

   namelist /nml1/ b1, b2
   namelist /nml2/ b3, b4

   open (1, file = 'array001d.1', form='formatted', access='sequential' )

   allocate ( b1(2), source = (/ base(1), base('abc') /) )
   allocate ( b2(2,2), source = reshape ( source = (/ base(data(21)), base(data(22)), base(data(23)) , base(data(24)) /), shape = (/2,2/) ) )
   b3= reshape ( source = (/ base(31), base('def'), base(data(31)), base(data(32)) /), shape =(/2,2/) )
   allocate ( b4(2,2), source = reshape ( source = (/ b1, base(2), base('ghi') /), shape=(/2,2/))  )

   write (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, data

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   select type ( g => dtv%u )
      type is (integer)
         write (unit, *, iostat=iostat )      g
      type is (character(*))
         write (unit, *, iostat=iostat )      g
      type is (data)
         write (unit, *, iostat=iostat )      g
   end select

   iomsg = 'dtiowrite'

end subroutine
