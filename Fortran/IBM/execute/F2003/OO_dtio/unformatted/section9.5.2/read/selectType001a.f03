! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to read associate name (from select type construct)
!*                                 with unlimited polymorphic entities
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

module m1
   type base
      character(3) :: c = ''
  contains
      procedure, pass :: get => getc
   end type

   type, extends(base) :: child
      character(3) :: cc = ''
   contains
      procedure, pass :: get => getcc
   end type

contains

   function getc (a)
      character(3) :: getc
      class(base), intent(in) :: a
      getc = a%c
   end function
   function getcc (a)
      character(3) :: getcc
      class(child), intent(in) :: a
      getcc = a%cc
   end function
end module

program selectType001a
   use m1

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(*), pointer     :: b1
   class(*), allocatable :: b2
   class(*), allocatable :: b3
   integer :: stat
   character(200) :: msg
   character(7) :: c1, c2
   character(4) :: c3

      ! allocation of variables

   allocate ( b1, source = child('xxx', 'xxx') )
   allocate ( b2, source = child('xxx','xxx') )
   allocate ( b3, source = base('xxx') )

   open (unit = 1, file ='selectType001.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )         'abcdef'
   write (1, iostat=stat, iomsg=msg )         'ghijkl'
   write (1, iostat=stat, iomsg=msg )         'mno'

   rewind 1

   select type (b11 => b1)
      class is (base)
         read (1, iostat=stat, iomsg=msg )    b11     !<= write b11%c and b11%cc
         if ( ( stat /= 0 ).or. ( msg /= 'dtio' ) )     error stop 1_4
         if ( b11%c      /= 'abc' )                     error stop 2_4
         if ( b11%get()  /= 'def' )                     error stop 3_4    !<- get() calls getcc()
         msg = ''
      class default
         error stop 4_4
   end select

   select type (b12 => b2)
      class is (child)
         read (1, iostat=stat, iomsg=msg )    b12     !<= write b12%c and b12%cc
         if ( ( stat /= 0 ).or. ( msg /= 'dtio' ) )     error stop 5_4
         if ( b12%c      /= 'ghi' )                     error stop 6_4
         if ( b12%get()  /= 'jkl' )                     error stop 7_4    !<- get() calls getcc()
         msg = ''
      class default
         error stop 8_4
   end select

   select type (b13 => b3)
      class is (base)
         read (1, iostat=stat, iomsg=msg )    b13     !<= write only b13%c
         if ( ( stat /= 0 ).or. ( msg /= 'dtio' ) )     error stop 9_4
         if ( b13%get()      /= 'mno' )                 error stop 10_4    !<- get() calls getc()
         msg = ''
      class default
         error stop 11_4
   end select

   select type (b1)
      class is (base)
         if ( b1%c      /= 'abc' )                     error stop 12_4
         if ( b1%get()  /= 'def' )                     error stop 13_4    !<- get() calls getcc()
      class default
         error stop 14_4
   end select

   select type (b2)
      class is (child)
         if ( b2%c      /= 'ghi' )                     error stop 15_4
         if ( b2%get()  /= 'jkl' )                     error stop 16_4    !<- get() calls getcc()
      class default
         error stop 17_4
   end select

   select type (b3)
      class is (base)
         if ( b3%get() /= 'mno' )                      error stop 18_4    !<- get() calls getc()
      class default
         error stop 19_4
   end select

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

    if ( iostat /= 0 ) error stop 20_4

    select type (dtv)
       type is (child)
          read (unit, iostat=iostat, iomsg=iomsg ) dtv%cc
    end select

   iomsg = 'dtio'

end subroutine