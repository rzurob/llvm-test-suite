! GB DTP extension using:
! ftcx_dtp -qck -qdeferredlp -qreuse=base /tstdev/F2003/mxminAll/con/maxMin/mxminLiteralArrObj2.f
! opt variations: -qnock -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with literal as selector
!*                               in associate construct
!*
!* ===================================================================

  program mxminLiteralArrObj2

   type base(k1,n1)    ! (1,3)
     integer, kind             :: k1
     integer, len              :: n1
     character(kind=k1,len=n1)    bname(2,2)
   end type

   type, extends (base) :: child    ! (1,3)
     character(kind=k1,len=n1) :: cname(2,2)
   end type

   class(child(1,:)), allocatable :: z(:)

   allocate(z(3), source=(/child(1,3)(max(reshape((/"aaa","aaa","aaa","aaa"/), (/2,2/)), reshape((/"bbb","bbb","bbb","bbb"/), (/2,2/))), reshape((/"ddd","ddd","ddd","ddd"/), (/2,2/))), child(1,3)(reshape((/"eee","eee","eee","eee"/), (/2,2/)), min(reshape((/"ccc","ccc","ccc","ccc"/), (/2,2/)), reshape((/"ddd","ddd","ddd","ddd"/), (/2,2/)))), child(1,3)(max(reshape((/"eee","eee","eee","eee"/), (/2,2/)), reshape((/"fff","fff","fff","fff"/), (/2,2/)), reshape((/"aaa","aaa","aaa","aaa"/), (/2,2/))),reshape((/"ddd","ddd","ddd","ddd"/), (/2,2/)))/))

   call associate1(max(z(1)%cname, z(2)%bname))

   call associate2(As2 = min(z(1)%cname, z(2)%bname, z(3)%cname))

   call associate3 (max(z(1)%cname(1,2), z(2)%bname(1,2)))

   call associate4 (min(z(1)%cname(1,2)(1:2), z(2)%bname(1,2)(1:2), z(3)%cname(1,2)(1:2)))
   deallocate(z)

   contains

!   associate(As1 => max(z(1)%cname, z(2)%bname))
   subroutine associate1(As1)
    character(3), intent(in) :: as1(2,2)

       if(any(As1 .ne. "eee") )then
         error stop 1_4
       end if

   end subroutine

!   associate(As2 => min(z(1)%cname, z(2)%bname, z(3)%cname))
   subroutine associate2(As2)
    character(*), intent(in) :: as2(:,:)

       if(any(As2 .ne. "ddd")) then
         error stop 2_4
        endif

   end subroutine

!   associate(As3 => max(z(1)%cname(1,2), z(2)%bname(1,2)))
   subroutine associate3(As3)
    character(*), intent(in) :: as3

       if(As3 .ne. "eee")then
         error stop 3_4
       end if

   end subroutine

!   associate(As4 => min(z(1)%cname(1,2)(1:2), z(2)%bname(1,2)(1:2), z(3)%cname(1,2)(1:2)))
   subroutine associate4(As4)
    character(2), intent(in) :: as4

       if(As4 .ne. "dd") then
         error stop 4_4
        endif
   end subroutine

  end program mxminLiteralArrObj2

