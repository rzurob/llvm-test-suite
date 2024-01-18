! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : associate002l
!*
!*  PROGRAMMER                 : David Forster (derived from associate002 by Robert Ma)
!*  DATE                       : 2007-09-14 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to read associate-name, and associate name is of unlimited polymorphic type
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

   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = 'xxx'
   end type

   type, extends(base) :: child
      class(*), pointer :: u => null()
   end type

end module


program associate002l
   use m1

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
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

   c1 = 'abc'
   c2 = 'def'
   c3 = 'ghi'
   c4 = 'jkl'
   c5 = 'mno'
   c6 = 'pqr'
   c7 = 'stu'
   c8 = 'vwx'
   i1 = 101

   ! allocation of variables

   allocate ( b1, source = child(3)('xxx') ) ! tcx: (3)
   allocate ( b2, source = child(3)('xxx') ) ! tcx: (3)
   allocate ( b3, source = child(3)('xxx') ) ! tcx: (3)
   allocate ( b4, source = child(3)('xxx') ) ! tcx: (3)

   select type ( b1 )
      type is (child(*)) ! tcx: (*)
         b1%u => b1%c
   end select
   select type ( b1 => b2 )
      type is (child(*)) ! tcx: (*)
         allocate ( b1%u, source = -999 )
   end select
   select type ( b1 => b3 )
      type is (child(*)) ! tcx: (*)
         allocate ( b1%u, source = 'xxx' )
   end select
   select type ( b1 => b4 )
      type is (child(*)) ! tcx: (*)
         allocate ( b1%u, source = 'xxx' )
   end select

   open (unit = 1, file ='associate002l.data', form='unformatted', access='sequential')

   ! unformatted I/O operations
   write (1, iostat=stat, iomsg=msg )       c8
   write (1, iostat=stat, iomsg=msg )       c1, c2
   write (1, iostat=stat, iomsg=msg )       c3, i1
   write (1, iostat=stat, iomsg=msg )       c4, c5
   write (1, iostat=stat, iomsg=msg )       c6, c7
   
   rewind 1

   associate ( a => b1, b => b2, c => b3, d => b4 )
      select type (b1)
         type is (child(*)) ! tcx: (*)
            associate ( aa => b1%u )
               select type (aa)
                  type is (character(*))
                     read ( 1, iostat = stat , iomsg = msg ) aa
                     print *, aa
                  class default
                     error stop 2_4
               end select
            end associate
      end select

      select type (a)
         class is (base(*)) ! tcx: (*)
            read ( 1, iostat = stat , iomsg = msg ) a
            if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4
         class default
            error stop 4_4
      end select

      associate ( a => b )
         select type (a)
            class is (base(*)) ! tcx: (*)
               read ( 1, iostat = stat , iomsg = msg ) a
               if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 5_4
            class default
               error stop 6_4
         end select
      end associate
      select type (a => c)
         class is (base(*)) ! tcx: (*)
            read ( 1, iostat = stat , iomsg = msg ) a
            if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 7_4
         class default
            error stop 8_4
      end select

      associate ( a => d )
         select type (a)
            class is (base(*)) ! tcx: (*)
               read ( 1, iostat = stat , iomsg = msg ) a
               if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 9_4
            class default
               error stop 10_4
         end select
      end associate

   end associate

   ! print out values of each entity
   select type (b1)
      type is (child(*)) ! tcx: (*)
         print *, b1%c
         select type( g => b1%u )
            type is (character(*))
               print *,g
         end select
   end select

   select type (b2)
      type is (child(*)) ! tcx: (*)
         print *, b2%c
         select type( g => b2%u )
            type is (integer)
               print *,g
         end select
   end select

   select type (b3)
      type is (child(*)) ! tcx: (*)
         print *, b3%c
         select type( g => b3%u )
            type is (character(*))
               print *,g
         end select
   end select

   select type (b4)
      type is (child(*)) ! tcx: (*)
         print *, b4%c
         select type( g => b4%u )
            type is (character(*))
               print *,g
         end select
   end select
   
   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, iostat=iostat ) dtv%c

    select type ( dtv )
       type is (child(*)) ! tcx: (*)
          select type (dd => dtv%u )
             type is (character(*))
                read (unit, iostat=iostat ) dd
             type is (integer)
                read (unit, iostat=iostat ) dd
             class default
                error stop 20_4
          end select
    end select
    iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 6 changes
! type: child - added parameters () to invoke with (3) / declare with (*) - 14 changes
