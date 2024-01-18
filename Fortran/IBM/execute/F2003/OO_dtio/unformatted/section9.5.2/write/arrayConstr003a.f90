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
! %GROUP: arrayConstr002.f
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
!*  DESCRIPTION                : Testing: Section 9.5.2 (Data Transfer input/output list)
!*                               - output item is an array constructor with unlimited polymorphic mixed
!*                                 with polymorphic and non-polymorphic variables
!*                               Sequential Access
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1

   type base
      character(3) :: c = ''
   end type

   type, extends(base) :: child
      integer(4) :: i = -999
   end type

end module


program arrayConstr002
   use m1

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables

   integer :: stat
   character(200) :: msg

   character(3) :: s1, s2, s3, s4, s5, s6
   character(3) :: s11, s12, s13, s14, s15, s16
   character(3) :: s21, s22, s23, s24, s25, s26
   character(3) :: s31, s32, s33, s34, s35, s36

   integer :: i1, i2, i3, i4, i5, i6
   integer :: i11, i12, i13, i14, i15, i16
   integer :: i21, i22, i23, i24, i25, i26

   class(*), allocatable  :: u1,u2(:)
   class(base), pointer   :: b1(:)
   class(child), pointer  :: c1(:)

   type(base) :: b2(2) = (/ base('abc'), base('def') /)
   type(child):: c2(2) = (/ child('aaa', 1001), child('bbb', 1002) /)

   ! allocation of variables

   allocate ( u1, source = child('abc',101) )
   allocate ( u2(3), source = (/ child('ABC',102), child('DEF',103), child('GHI',104)/) )
   allocate ( b1(2), source = (/ child('JKL',105), child('MNO',106) /) )
   allocate ( c1(2), source = (/ child('PQR',107), child('STU',108) /) )

   open (unit = 1, file ='arrayConstr002.data', form='unformatted', access='sequential')

   ! I/O operations

   select type ( u1 )
      class is (base)
         select type ( u2 )
            class is (base)
               write (1, iostat=stat, iomsg=msg ) (/ u1,u2,b1 /)
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4
         end select
   end select

   select type ( u1 )
      class is (child)
         select type ( u2 )
            class is (child)
               write (1, iostat=stat, iomsg=msg ) (/ u1,u2,c1 /)
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4
         end select
   end select

   select type ( u1 )
      type is (child)
         select type ( u2 )
            type is (child)
               write (1, iostat=stat, iomsg=msg ) (/ u1,u2,c2 /)
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4
         end select
   end select

   deallocate( u1, u2 )
   allocate ( u1, source = base('abc') )
   allocate ( u2(3), source = (/ base('ABC'), base('DEF'), base('GHI')/) )

   select type ( u1 )
      type is (base)
         select type ( u2 )
            type is (base)
               write (1, iostat=stat, iomsg=msg ) (/ u1,u2,b2 /)
               if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4
         end select
   end select

   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 5_4

   rewind 1

   read (1, iostat=stat, iomsg=msg )              s1, i1, s2, i2, s3, i3, s4, i4, s5, i5, s6, i6
   read (1, iostat=stat, iomsg=msg )              s11, i11, s12, i12, s13, i13, s14, i14, s15, i15, s16, i16
   read (1, iostat=stat, iomsg=msg )              s21, i21, s22, i22, s23, i23, s24, i24, s25, i25, s26, i26
   read (1, iostat=stat, iomsg=msg )              s31, s32, s33, s34, s35, s36

   ! check if the values are set correctly

   if ( ( s1 /= 'abc' ) .or. ( i1 /= 101 ) .or. &
        ( s2 /= 'ABC' ) .or. ( i2 /= 102 ) .or. &
        ( s3 /= 'DEF' ) .or. ( i3 /= 103 ) .or. &
        ( s4 /= 'GHI' ) .or. ( i4 /= 104 ) .or. &
        ( s5 /= 'JKL' ) .or. ( i5 /= 105 ) .or. &
        ( s6 /= 'MNO' ) .or. ( i6 /= 106 ) )    error stop 6_4

   if ( ( s11 /= 'abc' ) .or. ( i11 /= 101 ) .or. &
        ( s12 /= 'ABC' ) .or. ( i12 /= 102 ) .or. &
        ( s13 /= 'DEF' ) .or. ( i13 /= 103 ) .or. &
        ( s14 /= 'GHI' ) .or. ( i14 /= 104 ) .or. &
        ( s15 /= 'PQR' ) .or. ( i15 /= 107 ) .or. &
        ( s16 /= 'STU' ) .or. ( i16 /= 108 ) )  error stop 7_4

   if ( ( s21 /= 'abc' ) .or. ( i21 /= 101 ) .or. &
        ( s22 /= 'ABC' ) .or. ( i22 /= 102 ) .or. &
        ( s23 /= 'DEF' ) .or. ( i23 /= 103 ) .or. &
        ( s24 /= 'GHI' ) .or. ( i24 /= 104 ) .or. &
        ( s25 /= 'aaa' ) .or. ( i25 /= 1001 ) .or. &
        ( s26 /= 'bbb' ) .or. ( i26 /= 1002 ) )  error stop 8_4

   if ( ( s31 /= 'abc' ) .or. &
        ( s32 /= 'ABC' ) .or. &
        ( s33 /= 'DEF' ) .or. &
        ( s34 /= 'GHI' ) .or. &
        ( s35 /= 'abc' ) .or. &
        ( s36 /= 'def' )  )  error stop 9_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type ( dtv )
      type is (base)
         write (unit, iostat=iostat, iomsg=iomsg ) dtv%c
      type is (child)
         write (unit, iostat=iostat, iomsg=iomsg ) dtv%c, dtv%i
   end select

   iomsg = 'dtiowrite'

end subroutine
