! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to write associate-name, and associate name is of unlimited polymorphic type
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
      character(3) :: c = 'xxx'
   end type

   type, extends(base) :: child
      class(*), pointer :: u => null()
   end type

end module


program associate002
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
   class(*), pointer :: b1, b2
   class(*), allocatable :: b3, b4

   integer :: stat
   character(200) :: msg
   character(3) :: c1, c2, c3, c4, c5, c6, c7, c8
   integer(4) :: i1

   ! allocation of variables

   allocate ( b1, source = child('abc') )
   allocate ( b2, source = child('def') )
   allocate ( b3, source = child('ghi') )
   allocate ( b4, source = child('jkl') )

   select type ( b1 )
      type is (child)
         allocate ( b1%u, source = b1%c )
   end select
   select type ( b1 => b2 )
      type is (child)
         allocate ( b1%u, source = 123 )
   end select
   select type ( b1 => b3 )
      type is (child)
         allocate ( b1%u, source = 'ibm' )
   end select
   select type ( b1 => b4 )
      type is (child)
         allocate ( b1%u, source = 'ftn' )
   end select

   open (unit = 1, file ='associate002.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   associate ( a => b1, b => b2, c => b3, d => b4 )
      select type (b1)
         type is (child)
            associate ( aa => b1%u )
               select type (aa)
                  type is (character(*))
                     write ( 1, iostat = stat , iomsg = msg ) aa
                  class default
                     error stop 2_4
               end select
            end associate
      end select

      select type (a)
         class is (base)
            write ( 1, iostat = stat , iomsg = msg ) a
            if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 3_4
         class default
            error stop 4_4
      end select

      associate ( a => b )
         select type (a)
            class is (base)
               write ( 1, iostat = stat , iomsg = msg ) a
               if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 5_4
            class default
               error stop 6_4
         end select
      end associate
      select type (a => c)
         class is (base)
            write ( 1, iostat = stat , iomsg = msg ) a
            if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 7_4
         class default
            error stop 8_4
      end select

      associate ( a => d )
         select type (a)
            class is (base)
               write ( 1, iostat = stat , iomsg = msg ) a
               if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 9_4
            class default
               error stop 10_4
         end select
      end associate

   end associate

   rewind 1

   read (1, iostat=stat, iomsg=msg )       c8
   read (1, iostat=stat, iomsg=msg )       c1, c2
   read (1, iostat=stat, iomsg=msg )       c3, i1
   read (1, iostat=stat, iomsg=msg )       c4, c5
   read (1, iostat=stat, iomsg=msg )       c6, c7

   ! check if the values are set correctly

   if ( c1 /= 'abc' )        error stop 11_4
   if ( c2 /= 'abc' )        error stop 12_4
   if ( c3 /= 'def' )        error stop 13_4
   if ( i1 /=  123  )        error stop 14_4
   if ( c4 /= 'ghi' )        error stop 15_4
   if ( c5 /= 'ibm' )        error stop 16_4
   if ( c6 /= 'jkl' )        error stop 17_4
   if ( c7 /= 'ftn' )        error stop 18_4
   if ( c8 /= 'abc' )        error stop 19_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat ) dtv%c

    select type ( dtv )
       type is (child)
          select type (dd => dtv%u )
             type is (character(*))
                write (unit, iostat=iostat ) dd
             type is (integer)
                write (unit, iostat=iostat ) dd
             class default
                error stop 20_4
          end select
    end select
    iomsg = 'dtiowrite'

end subroutine
