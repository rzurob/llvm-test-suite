! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-06-23 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic/nonpoly
!*                                        array with abstract type with polymorphic derived type component(input)
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

   type :: data (kd)
      integer, kind :: kd
      integer(kd) :: i
   end type

   type, extends(data) :: childdata (lc)
      integer, len :: lc
      character(lc) :: c
   end type

   type, abstract :: base (kb)
      integer, kind :: kb
      class(data(kb)), pointer     :: d1
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      class(data(kc)), allocatable :: d2
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
  end interface

end module

program array101ckl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4)), allocatable :: b1(:)
   type(child(4,4))              :: b3(2,2)
   class(child(4,4)), pointer    :: b4(:,:)
   class(base(4)), pointer :: b2(:,:)
   namelist /nml1/ b2
   namelist /nml2/ b1, b3
   namelist /nml2/ b4

   open (1, file = 'array101ckl.1', form='formatted', access='stream' )
   allocate ( child(4,4) :: b1(2), b2(2,2) )
   select type ( b1 )
      type is (child(4,4))
         allocate ( childdata(4,3) :: b1(1)%d1,b1(1)%d2, b1(2)%d1, b1(2)%d2 )
   end select

   select type ( b2 )
      type is (child(4,4))
         allocate ( childdata(4,3) ::  b2(1,1)%d1, b2(1,1)%d2, b2(2,1)%d1,  b2(2,1)%d2, b2(1,2)%d1, b2(1,2)%d2, b2(2,2)%d1, b2(2,2)%d2 )
   end select

   allocate ( childdata(4,3) :: b3(1,1)%d1, b3(1,1)%d2,  b3(2,1)%d1,  b3(2,1)%d2, b3(1,2)%d1,  b3(1,2)%d2, b3(2,2)%d1, b3(2,2)%d2 )
   allocate ( b4(2,2) )
   allocate ( childdata(4,3) :: b4(1,1)%d1, b4(1,1)%d2,  b4(2,1)%d1,  b4(2,1)%d2, b4(1,2)%d1,  b4(1,2)%d2, b4(2,2)%d1, b4(2,2)%d2 )

   read (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   select type ( b1 )
      type is (child(4,4))
         select type ( g => b1(1)%d1 )
            type is (childdata(4,*))
               if  ( ( g%i /= 101 ) .or. ( g%c /= 'abc' ) )  error stop 3_4
         end select
         select type ( g => b1(1)%d2 )
            type is (childdata(4,*))
               if  ( ( g%i /= 1001 ) .or. ( g%c /= 'ABC' ) ) error stop 3_4
         end select
         select type ( g => b1(2)%d1 )
            type is (childdata(4,*))
               if  ( ( g%i /= 102 ) .or. ( g%c /= 'def' ) )  error stop 3_4
         end select
         select type ( g => b1(2)%d2 )
            type is (childdata(4,*))
               if  ( ( g%i /= 1002 ) .or. ( g%c /= 'DEF' ) ) error stop 3_4
         end select
   end select

   select type ( b2 )
      type is (child(4,4))
         select type ( g => b2(1,1)%d1 )
            type is (childdata(4,*))
               if  ( ( g%i /= 201 ) .or. ( g%c /= 'abc' ) )  error stop 4_4
         end select
         select type ( g => b2(1,1)%d2 )
            type is (childdata(4,*))
               if  ( ( g%i /= 2001 ) .or. ( g%c /= 'ABC' ) ) error stop 4_4
         end select
         select type ( g => b2(2,1)%d1 )
            type is (childdata(4,*))
               if  ( ( g%i /= 202 ) .or. ( g%c /= 'def' ) )  error stop 4_4
         end select
         select type ( g => b2(2,1)%d2 )
            type is (childdata(4,*))
               if  ( ( g%i /= 2002 ) .or. ( g%c /= 'DEF' ) ) error stop 4_4
         end select
         select type ( g => b2(1,2)%d1 )
            type is (childdata(4,*))
               if  ( ( g%i /= 203 ) .or. ( g%c /= 'ghi' ) )  error stop 4_4
         end select
         select type ( g => b2(1,2)%d2 )
            type is (childdata(4,*))
               if  ( ( g%i /= 2003 ) .or. ( g%c /= 'GHI' ) ) error stop 4_4
         end select
         select type ( g => b2(2,2)%d1 )
            type is (childdata(4,*))
               if  ( ( g%i /= 204 ) .or. ( g%c /= 'jkl' ) )  error stop 4_4
         end select
         select type ( g => b2(2,2)%d2 )
            type is (childdata(4,*))
               if  ( ( g%i /= 2004 ) .or. ( g%c /= 'JKL' ) ) error stop 4_4
         end select
   end select

   select type ( g => b3(1,1)%d1 )
      type is (childdata(4,*))
         if  ( ( g%i /= 301 ) .or. ( g%c /= 'abc' ) )  error stop 5_4
   end select
   select type ( g => b3(1,1)%d2 )
      type is (childdata(4,*))
         if  ( ( g%i /= 3001 ) .or. ( g%c /= 'ABC' ) ) error stop 5_4
   end select
   select type ( g => b3(2,1)%d1 )
      type is (childdata(4,*))
         if  ( ( g%i /= 302 ) .or. ( g%c /= 'def' ) )  error stop 5_4
   end select
   select type ( g => b3(2,1)%d2 )
      type is (childdata(4,*))
         if  ( ( g%i /= 3002 ) .or. ( g%c /= 'DEF' ) ) error stop 5_4
   end select
   select type ( g => b3(1,2)%d1 )
      type is (childdata(4,*))
         if  ( ( g%i /= 303 ) .or. ( g%c /= 'ghi' ) )  error stop 5_4
   end select
   select type ( g => b3(1,2)%d2 )
      type is (childdata(4,*))
         if  ( ( g%i /= 3003 ) .or. ( g%c /= 'GHI' ) ) error stop 5_4
   end select
   select type ( g => b3(2,2)%d1 )
      type is (childdata(4,*))
         if  ( ( g%i /= 304 ) .or. ( g%c /= 'jkl' ) )  error stop 5_4
   end select
   select type ( g => b3(2,2)%d2 )
      type is (childdata(4,*))
         if  ( ( g%i /= 3004 ) .or. ( g%c /= 'JKL' ) ) error stop 5_4
   end select

   select type ( b4 )
      type is (child(4,4))
         select type ( g => b4(1,1)%d1 )
            type is (childdata(4,*))
               if  ( ( g%i /= 401 ) .or. ( g%c /= 'abc' ) )  error stop 6_4
         end select
         select type ( g => b4(1,1)%d2 )
            type is (childdata(4,*))
               if  ( ( g%i /= 4001 ) .or. ( g%c /= 'ABC' ) ) error stop 6_4
         end select
         select type ( g => b4(2,1)%d1 )
            type is (childdata(4,*))
               if  ( ( g%i /= 402 ) .or. ( g%c /= 'def' ) )  error stop 6_4
         end select
         select type ( g => b4(2,1)%d2 )
            type is (childdata(4,*))
               if  ( ( g%i /= 4002 ) .or. ( g%c /= 'DEF' ) ) error stop 6_4
         end select
         select type ( g => b4(1,2)%d1 )
            type is (childdata(4,*))
               if  ( ( g%i /= 403 ) .or. ( g%c /= 'ghi' ) )  error stop 6_4
         end select
         select type ( g => b4(1,2)%d2 )
            type is (childdata(4,*))
               if  ( ( g%i /= 4003 ) .or. ( g%c /= 'GHI' ) ) error stop 6_4
         end select
         select type ( g => b4(2,2)%d1 )
            type is (childdata(4,*))
               if  ( ( g%i /= 404 ) .or. ( g%c /= 'jkl' ) )  error stop 6_4
         end select
         select type ( g => b4(2,2)%d2 )
            type is (childdata(4,*))
               if  ( ( g%i /= 4004 ) .or. ( g%c /= 'JKL' ) ) error stop 6_4
         end select
   end select

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child, data, childdata

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 7_4
   if ( size(v_list, 1) /= 0 ) error stop 8_4

   select type ( m => dtv )
      class is (base(4))
         error stop 9_4
      type is (child(4,4))
         select type ( d1 => m%d1 )
            type is (data(4))
               error stop 10_4
            type is (childdata(4,*))
               read (unit, *, iostat=iostat )      d1%i, d1%c
         end select
         select type ( d2 => m%d2 )
            type is (data(4))
               error stop 11_4
            type is (childdata(4,*))
               read (unit, *, iostat=iostat )      d2%i, d2%c
         end select
   end select

   iomsg = 'dtioread'

end subroutine
