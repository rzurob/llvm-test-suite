!#######################################################################
! OO_dtio/formatted/section10.10/array101b.f, xlftest.OO_dtio, tstdev, 1.1
! Extract Date/Time: 05/03/17 16:42:29
! Checkin Date/Time: 05/03/01 15:58:00
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: array101b.f
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
!*                                        Try namelist formatting with polymorphic/nonpoly array with type with derived type component (Input)
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
      integer :: i = 999
   end type

   type :: base
      type(data) :: d1
   end type

   type, extends(base) :: child
      type(data) :: d2
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
  end interface

   class(base), pointer :: b2(:,:)

end module

program array101b
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1(:)
   type(child)              :: b3(2,2)
   class(child), pointer    :: b4(:,:)

   namelist /nml1/ b1
   namelist /nml2/ b2
   namelist /nml3/ b3
   namelist /nml4/ b4

   open (1, file = 'array101b.1', form='formatted', access='stream' )
   allocate(child :: b1(2))
   allocate(b4(2,2), source = b3 )
   allocate( child :: b2(2,2) )

   read (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read (1,NML=nml3, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   read (1,NML=nml4, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4

   select type (b1)
      type is (child)
         if ( ( b1(1)%d1%i /= 101 ) .or. ( b1(1)%d2%i /= 102 )  .or. &
              ( b1(2)%d1%i /= 103 ) .or. ( b1(2)%d2%i /= 104 ) )    error stop 5_4
   end select

   select type (b2)
      type is (child)
         if ( ( b2(1,1)%d1%i /= 201 ) .or. ( b2(1,1)%d2%i /= 202 )  .or. &
              ( b2(2,1)%d1%i /= 999 ) .or. ( b2(2,1)%d2%i /= 999 )  .or. &
              ( b2(1,2)%d1%i /= 999 ) .or. ( b2(1,2)%d2%i /= 999 )  .or. &
              ( b2(2,2)%d1%i /= 203 ) .or. ( b2(2,2)%d2%i /= 204 ) )    error stop 6_4
   end select

   if ( ( b3(1,1)%d1%i /= 301 ) .or. ( b3(1,1)%d2%i /= 302 )  .or. &
        ( b3(2,1)%d1%i /= 305 ) .or. ( b3(2,1)%d2%i /= 306 )  .or. &
        ( b3(1,2)%d1%i /= 303 ) .or. ( b3(1,2)%d2%i /= 304 )  .or. &
        ( b3(2,2)%d1%i /= 307 ) .or. ( b3(2,2)%d2%i /= 308 ) )    error stop 7_4

   if ( ( b4(1,1)%d1%i /= 405 ) .or. ( b4(1,1)%d2%i /= 406 )  .or. &
        ( b4(2,1)%d1%i /= 407 ) .or. ( b4(2,1)%d2%i /= 408 )  .or. &
        ( b4(1,2)%d1%i /= 401 ) .or. ( b4(1,2)%d2%i /= 402 )  .or. &
        ( b4(2,2)%d1%i /= 403 ) .or. ( b4(2,2)%d2%i /= 404 ) )    error stop 8_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   select type ( m => dtv )
      class is (base)
         read (unit, *, iostat=iostat )      m%d1
      type is (child)
         read (unit, *, iostat=iostat )      m%d1
         read (unit, *, iostat=iostat )      m%d2
   end select

   iomsg = 'dtioread'

end subroutine
