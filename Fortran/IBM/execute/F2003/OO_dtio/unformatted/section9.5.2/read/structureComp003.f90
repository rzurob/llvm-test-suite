! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try input item to be structure component, try parent component
!*                               Direct Access
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
         procedure, pass :: getC
   end type

   type, extends(base) :: child
      character(3) :: cc = ''
   end type

   type container
      type(child) :: b1
      type(child) :: b2
   end type

contains
   function getC (a)
      class(base), intent(in) :: a
      character(3) :: getC
      getC = a%c
   end function
end module


program structureComp003
   use m1

   interface read(unformatted)

      subroutine readUnformattedContainer (dtv, unit, iostat, iomsg)
         import container
         class(container), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine

      subroutine readUnformattedBase (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine

   end interface

   ! declaration of variables
   class(container), allocatable  :: b11
   class(container), pointer      :: b12
   type (container)               :: b13
   integer :: stat
   character(200) :: msg

   ! allocation of variables
   allocate ( b11, source = container( b2=child('xxx','xxx'), b1=child('xxx','xxx') ) )
   allocate ( b12, source = container( b2=child('xxx','xxx'), b1=child('xxx','xxx') ) )
   b13 = container( b2=child('xxx','xxx'), b1=child('xxx','xxx') )

   open (unit = 1, file ='structureComp003.data', form='unformatted', access='direct', recl= 20)

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg, rec = 1 )             'abcdefjjjjjj'
   write (1, iostat=stat, iomsg=msg, rec = 19 )            'ghijkl'
   write (1, iostat=stat, iomsg=msg, rec = 13 )            'mnojjj'

   read (1, iostat=stat, iomsg=msg, rec = 1 )             b11%b1%base, b11%b2%base
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )                     error stop 1_4
      if ( ( b11%b1%c /= 'abc' ) .or. ( b11%b1%cc /= 'xxx' )              .or. &
           ( b11%b2%c /= 'def' ) .or. ( b11%b2%cc /= 'xxx' ))             error stop 2_4
      msg = ''

   read (1, iostat=stat, iomsg=msg, rec=19 )             b12%b1
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )                     error stop 3_4
      if ( ( b12%b1%c /= 'ghi' ) .or. ( b12%b1%cc /= 'jkl' )              .or. &
           ( b12%b2%c /= 'xxx' ) .or. ( b12%b2%cc /= 'xxx' ))             error stop 4_4
      msg = ''

   read (1, iostat=stat, iomsg=msg, rec = 13)             b13%b2%base
      if ( ( stat /= 0 ) .or. ( msg /= 'basedtio' ) )                     error stop 5_4
      if ( ( b13%b1%c /= 'xxx' ) .or. ( b13%b1%cc /= 'xxx' )              .or. &
           ( b13%b2%c /= 'mno' ) .or. ( b13%b2%cc /= 'xxx' ))             error stop 6_4
      msg = ''

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformattedContainer (dtv, unit, iostat, iomsg)
use m1
    class(container), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    interface read(unformatted)
        subroutine readUnformattedBase (dtv, unit, iostat, iomsg)
            import base
            class(base), intent(inout) :: dtv
            integer,  intent(in) :: unit
            integer,  intent(out) :: iostat
            character(*),  intent(inout) :: iomsg
        end subroutine
    end interface

    read (unit, iostat=iostat, iomsg=iomsg ) dtv%b1, dtv%b2

    if ( (iostat /= 0 ) .or. ( iomsg /= 'basedtio' ))       error stop 7_4

    iomsg = 'containerdtio'

end subroutine

subroutine readUnformattedBase (dtv, unit, iostat, iomsg)
use m1
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

   if ( iostat /= 0 ) error stop 8_4

   select type (dtv)
      type is (child)
         read (unit, iostat=iostat, iomsg=iomsg ) dtv%cc
   end select

   iomsg = 'basedtio'

end subroutine
