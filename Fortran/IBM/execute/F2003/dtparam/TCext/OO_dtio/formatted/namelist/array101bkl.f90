! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : array101bkl
!*
!*  PROGRAMMER                 : David Forster (derived from array101b by Robert Ma)
!*  DATE                       : 2007-06-20 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with polymorphic/nonpoly array with type with derived type component (Input)
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
      integer(kd) :: i = 999
   end type

   type :: base (kb)
      integer, kind :: kb
      type(data(kb)) :: d1
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      type(data(kc)) :: d2
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

   class(base(4)), pointer :: b2(:,:)

end module

program array101bkl
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base(4)), allocatable :: b1(:)
   type(child(4,4))              :: b3(2,2)
   class(child(4,4)), pointer    :: b4(:,:)

   namelist /nml1/ b1
   namelist /nml2/ b2   
   namelist /nml3/ b3
   namelist /nml4/ b4

   open (1, file = 'array101bkl.1', form='formatted', access='stream' )
   allocate(child(4,4) :: b1(2))
   allocate(b4(2,2), source = b3 )
   allocate( child(4,4) :: b2(2,2) )

   read (1,NML=nml1, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   read (1,NML=nml2, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 2_4

   read (1,NML=nml3, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 3_4

   read (1,NML=nml4, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 4_4

   select type (b1)
      type is (child(4,4))
         if ( ( b1(1)%d1%i /= 101 ) .or. ( b1(1)%d2%i /= 102 )  .or. &
              ( b1(2)%d1%i /= 103 ) .or. ( b1(2)%d2%i /= 104 ) )    error stop 5_4
   end select

   select type (b2)
      type is (child(4,4))
         if ( ( b2(1,1)%d1%i /= 201 ) .or. ( b2(1,1)%d2%i /= 202 )  .or. &
              ( b2(2,1)%d1%i /= 999 ) .or. ( b2(2,1)%d2%i /= 999 )  .or. &
              ( b2(1,2)%d1%i /= 999 ) .or. ( b2(1,2)%d2%i /= 999 )  .or. &
              ( b2(2,2)%d1%i /= 203 ) .or. ( b2(2,2)%d2%i /= 204 ) )    error stop 6_4
   end select

   if ( ( b3(1,1)%d1%i /= 301 ) .or. ( b3(1,1)%d2%i /= 302 )  .or. &
        ( b3(2,1)%d1%i /= 305 ) .or. ( b3(2,1)%d2%i /= 306 )  .or. &
        ( b3(1,2)%d1%i /= 303 ) .or. ( b3(1,2)%d2%i /= 304 )  .or. &
        ( b3(2,2)%d1%i /= 307 ) .or. ( b3(2,2)%d2%i /= 308 ) )    error stop 7_4

   if ( ( b4(1,1)%d1%i /= 405 ) .or. ( b4(1,1)%d2%i /= 406 )  .or. &
        ( b4(2,1)%d1%i /= 407 ) .or. ( b4(2,1)%d2%i /= 408 )  .or. &
        ( b4(1,2)%d1%i /= 401 ) .or. ( b4(1,2)%d2%i /= 402 )  .or. &
        ( b4(2,2)%d1%i /= 403 ) .or. ( b4(2,2)%d2%i /= 404 ) )    error stop 8_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base(4)), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 3_4
   if ( size(v_list, 1) /= 0 ) error stop 4_4

   select type ( m => dtv )
      class is (base(4))
         read (unit, *, iostat=iostat )      m%d1
      type is (child(4,4))
         read (unit, *, iostat=iostat )      m%d1
         read (unit, *, iostat=iostat )      m%d2
   end select

   iomsg = 'dtioread'

end subroutine
