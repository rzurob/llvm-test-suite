!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAXVAL/MINVAL(derived type component as its
!*                               argument with logical, byte, typeless to DIM
!*                               and/or MASK.
!* ===================================================================
@process intlog

program mxminvalVarConform02

      type dt
          character(3) :: x(2,3) = reshape((/"aaa","bbb","ccc","ddd","eee","fff"/),(/2,3/))
          integer :: v1(2,3) = 1
          logical :: v2(2,3) = .true.
          byte, dimension(2,3) :: v3 = reshape((/1,1,1,1,1,1/), (/2,3/))
      end type

      character(3) v(2), vv(3)

      type(dt) :: dt_object

      if(maxval(dt_object%x) .ne. "fff") error stop 1_4

      if(maxval(dt_object%x, mask = dt_object%v1) .ne. "fff") error stop 2_4

      if(minval(dt_object%x, mask = dt_object%v2) .ne. "aaa") error stop 3_4

      if(minval(dt_object%x, mask = dt_object%v3) .ne. "aaa") error stop 4_4

      v = maxval(dt_object%x, dim = 2, mask = dt_object%v1)

      if(v(1) .ne. "eee" .or. v(2) .ne. "fff") error stop 5_4

      v = "zzz"

      v = maxval(dt_object%x, dim = 2, mask = dt_object%v2)

      if(v(1) .ne. "eee" .or. v(2) .ne. "fff") error stop 6_4

      v = "kkk"

      v = maxval(dt_object%x, dim = 2, mask = dt_object%v3)

      if(v(1) .ne. "eee" .or. v(2) .ne. "fff") error stop 7_4

      if(maxval(dt_object%x, mask = b"011") .ne. "fff") error stop 8_4

      vv = maxval(dt_object%x, dim =1,  mask = b"011")

      if(vv(1) .ne. "bbb" .or. vv(2) .ne. "ddd" .or. vv(3) .ne. "fff") then
           error stop 9_4
      endif

      v = maxval(dt_object%x, dim = b"010", mask = dt_object%v3)

      if(v(1) .ne. "eee" .or. v(2) .ne. "fff") error stop 10_4

      v = "sss"

      v = maxval(dt_object%x, dim = b"010", mask = b"111")

      if(v(1) .ne. "eee" .or. v(2) .ne. "fff") error stop 11_4

      v = "zzz"

      v = minval(dt_object%x, dim = b"010", mask = dt_object%v2)

      if(v(1) .ne. "aaa" .or. v(2) .ne. "bbb") error stop 12_4

end program mxminvalVarConform02

