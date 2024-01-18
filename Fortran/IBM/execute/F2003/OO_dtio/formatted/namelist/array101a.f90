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
! %GROUP: array101a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
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
!*  DATE                       : 11/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic/nonpoly array (Input)
!*                                        where input data is of array-element and array section
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
   type base
      integer :: i = 999
   end type

   type, extends(base) :: child
      integer :: i1 = 999
   end type

   class(base), allocatable   :: b1(:,:)
   class(base), pointer       :: b2(:,:)
   type(child), allocatable   :: b4(:,:)
   class(child), pointer      :: b5(:,:)

end module

module m1
   use m
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
end module

program array101a
   use m1

   integer :: stat
   character(200) :: msg = ''
   type(base)                 :: b3(2,2)
   namelist /nml1/ b1, b2
   namelist /nml2/ b3
   namelist /nml3/ b4, b5

   open (1, file = 'array101a.1', form='formatted', access='stream' )
   allocate( child:: b1(2,2), b2(3,3))
   b3 = base()
   allocate(b4(2,2), b5(3,3))

   read (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read (1,NML=nml3, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   select type ( b1 )
      type is (child)
         if ( ( b1(1,1)%i /= 101 ) .or. ( b1(1,1)%i1 /= 102 ) .or. &
              ( b1(2,1)%i /= 103 ) .or. ( b1(2,1)%i1 /= 104 ) .or. &
              ( b1(1,2)%i /= 999 ) .or. ( b1(1,2)%i1 /= 999 ) .or. &
              ( b1(2,2)%i /= 105 ) .or. ( b1(2,2)%i1 /= 106 ) ) error stop 4_4
   end select

   select type ( b2 )
      type is (child)
         if ( ( b2(1,1)%i /= 201 ) .or. ( b2(1,1)%i1 /= 202 ) .or. &
              ( b2(2,1)%i /= 203 ) .or. ( b2(2,1)%i1 /= 204 ) .or. &
              ( b2(3,1)%i /= 205 ) .or. ( b2(3,1)%i1 /= 206 ) .or. &
              ( b2(1,2)%i /= 207 ) .or. ( b2(1,2)%i1 /= 208 ) .or. &
              ( b2(2,2)%i /= 209 ) .or. ( b2(2,2)%i1 /= 210 ) .or. &
              ( b2(3,2)%i /= 999 ) .or. ( b2(3,2)%i1 /= 999 ) .or. &
              ( b2(1,3)%i /= 999 ) .or. ( b2(1,3)%i1 /= 999 ) .or. &
              ( b2(2,3)%i /= 999 ) .or. ( b2(2,3)%i1 /= 999 ) .or. &
              ( b2(3,3)%i /= 999 ) .or. ( b2(3,3)%i1 /= 999 ))      error stop 5_4
   end select
   
   if ( ( b3(1,1)%i /= 301 ) .or. ( b3(2,1)%i /= 302 ) .or. ( b3(1,2)%i /= 303 ) .or. ( b3(2,2)%i /= 304 ) ) error stop 6_4

   if ( ( b4(1,1)%i /= 401 ) .or. ( b4(1,1)%i1 /= 402 ) .or. &
        ( b4(2,1)%i /= 403 ) .or. ( b4(2,1)%i1 /= 404 ) .or. &
        ( b4(1,2)%i /= 999 ) .or. ( b4(1,2)%i1 /= 999 ) .or. &
        ( b4(2,2)%i /= 999 ) .or. ( b4(2,2)%i1 /= 999 ) )           error stop 7_4

   if ( ( b5(1,1)%i /= 503 ) .or. ( b5(1,1)%i1 /= 504 ) .or. &
        ( b5(2,1)%i /= 999 ) .or. ( b5(2,1)%i1 /= 999 ) .or. &
        ( b5(3,1)%i /= 501 ) .or. ( b5(3,1)%i1 /= 502 ) .or. &
        ( b5(1,2)%i /= 508 ) .or. ( b5(1,2)%i1 /= 509 ) .or. &
        ( b5(2,2)%i /= 999 ) .or. ( b5(2,2)%i1 /= 999 ) .or. &
        ( b5(3,2)%i /= 506 ) .or. ( b5(3,2)%i1 /= 507 ) .or. &
        ( b5(1,3)%i /= 999 ) .or. ( b5(1,3)%i1 /= 999 ) .or. &
        ( b5(2,3)%i /= 999 ) .or. ( b5(2,3)%i1 /= 999 ) .or. &
        ( b5(3,3)%i /= 999 ) .or. ( b5(3,3)%i1 /= 999 ))            error stop 8_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 9_4
   if ( size(v_list, 1) /= 0 ) error stop 10_4

   select type (dtv)
      type is (base)
         read (unit, *, iostat=iostat )      dtv%i
      type is (child)
         read (unit, *, iostat=iostat )      dtv%i, dtv%i1
   end select
   iomsg = 'dtioread'

end subroutine
