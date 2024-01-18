!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: selectType002.f
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
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to read selector in class default in select type construct
!*                               Sequential Access
!*
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
      character(3) :: c = ''
   end type

   type, extends(base) :: child
      character(3) :: cc = ''
   end type

end module


module m1
   use m

   type, extends(child) :: gen3
      integer(4) :: i = -999
   end type

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program selectType002
   use m1

   ! declaration of variables
   class(base), pointer     :: b1
   class(base), allocatable :: b2, b3

   class(child), pointer    :: c1
   class(child), allocatable:: c2

   class(gen3), pointer     :: g1
   class(gen3), allocatable :: g2

   integer :: stat
   character(200) :: msg =''

   ! allocation of variables

   allocate ( b1, source = base () )
   allocate ( b2, source = child() )
   allocate ( b3, source = gen3 () )

   allocate ( c1, source = child() )
   allocate ( c2, source = gen3() )

   allocate ( g1, source = gen3() )
   allocate ( g2, source = gen3() )

   open (unit = 1, file ='selectType002.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write ( 1, iostat = stat, iomsg = msg)     'abc'
   write ( 1, iostat = stat, iomsg = msg)     'def', 'ghi'
   write ( 1, iostat = stat, iomsg = msg)     'jkl', 'mno', 101
   write ( 1, iostat = stat, iomsg = msg)     'ABC', 'DEF'
   write ( 1, iostat = stat, iomsg = msg)     'GHI', 'JKL', 201
   write ( 1, iostat = stat, iomsg = msg)     'ABc', 'DEf', 301
   write ( 1, iostat = stat, iomsg = msg)     'GHi', 'JKl', 302

   rewind 1
   msg = ''

   select type (g => b1)
      class default
         read (1, iostat=stat, iomsg=msg )    g
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') ) error stop 1_4
         if ( g%c /= 'abc' )                           error stop 2_4
   end select

   select type (b2)
      class default
         read (1, iostat=stat, iomsg=msg )    b2
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') ) error stop 3_4
   end select

   select type (b3)
      class default
         read (1, iostat=stat, iomsg=msg )    b3
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') ) error stop 4_4
   end select

   select type (g => c1)
      class default
         read (1, iostat=stat, iomsg=msg )    g
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') ) error stop 5_4
         if ( ( g%c /= 'ABC' ) .or. (g%cc /= 'DEF' ) ) error stop 6_4
   end select

   select type (c2)
      class default
         read (1, iostat=stat, iomsg=msg )    c2
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') ) error stop 7_4
   end select

   select type (g => g1)
      class default
         read (1, iostat=stat, iomsg=msg )    g
         if ( ( g%c /= 'ABc' ) .or. (g%cc /= 'DEf' ) .or. (g%i /= 301 ) ) error stop 8_4
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') ) error stop 9_4
   end select

   select type (g2)
      class default
         read (1, iostat=stat, iomsg=msg )    g2
         if ( ( g2%c /= 'GHi' ) .or. (g2%cc /= 'JKl' ) .or. (g2%i /= 302 ) ) error stop 10_4
         if ( ( stat /= 0 ) .or. (msg /= 'dtioread') ) error stop 11_4
   end select

   ! check if the values are set correctly

   select type ( b2 )
      type is (child)
         if ( ( b2%c /= 'def' ) .or. ( b2%cc /= 'ghi' ) ) error stop 12_4
      class default
         error stop 13_4
   end select

   select type ( b3 )
      type is (gen3)
         if ( ( b3%c /= 'jkl' ) .or. ( b3%cc /= 'mno' ) .or. ( b3%i /= 101 ) ) error stop 14_4
      class default
         error stop 15_4
   end select

   select type ( c2 )
      type is (gen3)
         if ( ( c2%c /= 'GHI' ) .or. ( c2%cc /= 'JKL' ) .or. ( c2%i /= 201 ) ) error stop 16_4
      class default
         error stop 17_4
   end select

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child, gen3
    class(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    select type (dtv)
       type is (base)
          read (unit, iostat=iostat ) dtv%c
       type is (child)
          read (unit, iostat=iostat ) dtv%c, dtv%cc
       type is (gen3)
          read (unit, iostat=iostat ) dtv%c, dtv%cc, dtv%i
    end select

    iomsg = 'dtioread'

end subroutine
