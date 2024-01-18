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
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2 (Data Transfer input/output list)
!*                               - output item is an array constructor with polymorphic variables
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

   character(3) :: s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13
   character(15) :: u1
   integer :: i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13

   class(base), allocatable  :: b1(:), b2
   class(child), pointer     :: c1(:,:), c2

   type(base)  :: b3, b4(3)
   type(child) :: c3, c4(2)

   ! allocation of variables

   allocate ( b1(3), source = (/ child('abc', 101), child('def', 102), child('ghi', 103) /) )
   allocate ( b2, source = child('ABC', 104) )

   allocate ( c1(2,2), source = reshape ( source = (/ child('abc', 201 ), child('def', 202 ), child('ghi', 203 ), child ('jkl', 204 ) /), shape = (/2,2/) ) )
   allocate ( c2, source = child('ABC', 205 ) )

   b3 = base('ABC')
   b4 = (/ base('AAA') , base('BBB'), base('CCC') /)

   c3 = child ( 'DEF', 301 )
   c4 = (/ child ( 'aaa', 302 ), child ( 'bbb', 303 ) /)

   open (unit = 1, file ='arrayConstr002.data', form='unformatted', access='sequential')

   ! I/O operations

   write (1, iostat=stat, iomsg=msg ) (/ b1, b2 /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   write (1, iostat=stat, iomsg=msg ) (/ b3, b4(1), b4(2:3), base('DDD') /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 2_4

   write (1, iostat=stat, iomsg=msg ) (/ reshape (source = c1, shape = (/4/) ), c2 /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4

   write (1, iostat=stat, iomsg=msg ) (/ c3, c4, child('ccc', 304) /)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 4_4

   rewind 1

   read (1, iostat=stat, iomsg=msg )              s1, i1, s2, i2, s3, i3, s4, i4
   read (1, iostat=stat, iomsg=msg )              u1
   read (1, iostat=stat, iomsg=msg )              s5, i5, s6, i6, s7, i7, s8, i8, s9, i9
   read (1, iostat=stat, iomsg=msg )              s10, i10, s11, i11, s12, i12, s13, i13

   ! check if the values are set correctly

   if ( ( s1 /= 'abc' ) .or. ( i1 /= 101 ) .or. &
        ( s2 /= 'def' ) .or. ( i2 /= 102 ) .or. &
        ( s3 /= 'ghi' ) .or. ( i3 /= 103 ) .or. &
        ( s4 /= 'ABC' ) .or. ( i4 /= 104 ) )   error stop 5_4

   if ( u1 /= 'ABCAAABBBCCCDDD' )              error stop 6_4

   if ( ( s5 /= 'abc' ) .or. ( i5 /= 201 ) .or. &
        ( s6 /= 'def' ) .or. ( i6 /= 202 ) .or. &
        ( s7 /= 'ghi' ) .or. ( i7 /= 203 ) .or. &
        ( s8 /= 'jkl' ) .or. ( i8 /= 204 ) .or. &
        ( s9 /= 'ABC' ) .or. ( i9 /= 205 ) )   error stop 7_4

   if ( ( s10 /= 'DEF' ) .or. ( i10 /= 301 ) .or. &
        ( s11 /= 'aaa' ) .or. ( i11 /= 302 ) .or. &
        ( s12 /= 'bbb' ) .or. ( i12 /= 303 ) .or. &
        ( s13 /= 'ccc' ) .or. ( i13 /= 304 ) ) error stop 8_4

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
