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
! %GROUP: selectType002.f
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
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to write selector in class default in select type construct
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
      integer(4) :: i
   end type

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
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

   character(3) :: cc1, cc2, cc3, cc4, cc5, cc6, cc7, cc8, cc9, cc10, cc11, cc12, cc13
   integer(4)   :: i1, i2, i3, i4

   ! allocation of variables

   allocate ( b1, source = base ('abc') )
   allocate ( b2, source = child('def','ghi') )
   allocate ( b3, source = gen3 ('jkl','mno', 101 ) )

   allocate ( c1, source = child('ABC', 'DEF') )
   allocate ( c2, source = gen3('GHI', 'JKL', 201 ) )

   allocate ( g1, source = gen3('ABc','DEf', 301) )
   allocate ( g2, source = gen3('GHi','JKl', 302) )

   open (unit = 1, file ='selectType002.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   select type (g => b1)
      class default
         write (1, iostat=stat, iomsg=msg )    g
         if ( ( stat /= 0 ) .or. (msg /= 'dtiowrite') ) error stop 1_4
   end select

   select type (b2)
      class default
         write (1, iostat=stat, iomsg=msg )    b2
         if ( ( stat /= 0 ) .or. (msg /= 'dtiowrite') ) error stop 2_4
   end select

   select type (b3)
      class default
         write (1, iostat=stat, iomsg=msg )    b3
         if ( ( stat /= 0 ) .or. (msg /= 'dtiowrite') ) error stop 3_4
   end select

   select type (g => c1)
      class default
         write (1, iostat=stat, iomsg=msg )    g
         if ( ( stat /= 0 ) .or. (msg /= 'dtiowrite') ) error stop 4_4
   end select

   select type (c2)
      class default
         write (1, iostat=stat, iomsg=msg )    c2
         if ( ( stat /= 0 ) .or. (msg /= 'dtiowrite') ) error stop 5_4
   end select

   select type (g => g1)
      class default
         write (1, iostat=stat, iomsg=msg )    g
         if ( ( stat /= 0 ) .or. (msg /= 'dtiowrite') ) error stop 6_4
   end select

   select type (g2)
      class default
         write (1, iostat=stat, iomsg=msg )    g2
         if ( ( stat /= 0 ) .or. (msg /= 'dtiowrite') ) error stop 7_4
   end select

   rewind 1

   msg = ''

   read (1, iostat=stat, iomsg=msg)       cc1
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 8_4
   read (1, iostat=stat, iomsg=msg)       cc2, cc3
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 9_4
   read (1, iostat=stat, iomsg=msg)       cc4, cc5, i1
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 10_4
   read (1, iostat=stat, iomsg=msg)       cc6, cc7
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 11_4
   read (1, iostat=stat, iomsg=msg)       cc8, cc9, i2
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 12_4
   read (1, iostat=stat, iomsg=msg)       cc10, cc11, i3
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 13_4
   read (1, iostat=stat, iomsg=msg)       cc12, cc13, i4
   if ( ( stat /= 0 ) .or. (msg /= '') )                error stop 14_4


   ! check if the values are set correctly

   if ( ( cc1 /= 'abc' ) .or. ( cc2 /= 'def' ) .or. ( cc3 /= 'ghi' ) .or. &
        ( cc4 /= 'jkl' ) .or. ( cc5 /= 'mno' ) .or. ( i1 /= 101 ) ) error stop 15_4

   if ( ( cc6 /= 'ABC' ) .or. ( cc7 /= 'DEF' ) .or. ( cc8 /= 'GHI' ) .or. &
        ( cc9 /= 'JKL' ) .or. ( i2 /= 201 ) )                       error stop 16_4

   if ( ( cc10 /= 'ABc' ) .or. ( cc11 /= 'DEf' ) .or. ( cc12 /= 'GHi' ) .or. &
        ( cc13 /= 'JKl' ) .or. ( i3 /= 301 ) .or. ( i4 /= 302 ) )   error stop 17_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child, gen3
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    select type (dtv)
       type is (base)
          write (unit, iostat=iostat ) dtv%c
       type is (child)
          write (unit, iostat=iostat ) dtv%c, dtv%cc
       type is (gen3)
          write (unit, iostat=iostat ) dtv%c, dtv%cc, dtv%i
    end select

    iomsg = 'dtiowrite'

end subroutine
