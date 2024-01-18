! Unit test for CAF
!
! %W%
! %H% %I%
!
! these buckets must be ran for specific image numbers
.im $(tsrcdir)/F2008/coarray/unit_test/fixed_num_images.tl

! the following are buckets that can be ran for different image numbers

! run with images number set dynamically
.im $(tsrcdir)/F2008/coarray/unit_test/dynamic_num_images.tl -xopts=:CAF_NUM_IMAGES=2 -xopts=:MP_PROCS=1
.im $(tsrcdir)/F2008/coarray/unit_test/dynamic_num_images.tl -xopts=:CAF_NUM_IMAGES=3 -xopts=:MP_PROCS=1
.im $(tsrcdir)/F2008/coarray/unit_test/dynamic_num_images.tl -xopts=:CAF_NUM_IMAGES=4 -xopts=:MP_PROCS=1
.im $(tsrcdir)/F2008/coarray/unit_test/dynamic_num_images.tl -xopts=:CAF_NUM_IMAGES=8 -xopts=:MP_PROCS=1

! run with images number set statically
.im $(tsrcdir)/F2008/coarray/unit_test/dynamic_num_images.tl -xopts=$(F_R_COMPILE^OPT_COARRAY_IMAGES<2>) $(F_R_COMPILE^OPT_COARRAY_DNODES<1>) -xopts=:MP_PROCS=1
.im $(tsrcdir)/F2008/coarray/unit_test/dynamic_num_images.tl -xopts=$(F_R_COMPILE^OPT_COARRAY_IMAGES<3>) $(F_R_COMPILE^OPT_COARRAY_DNODES<1>) -xopts=:MP_PROCS=1
.im $(tsrcdir)/F2008/coarray/unit_test/dynamic_num_images.tl -xopts=$(F_R_COMPILE^OPT_COARRAY_IMAGES<4>) $(F_R_COMPILE^OPT_COARRAY_DNODES<1>) -xopts=:MP_PROCS=1
.im $(tsrcdir)/F2008/coarray/unit_test/dynamic_num_images.tl -xopts=$(F_R_COMPILE^OPT_COARRAY_IMAGES<8>) $(F_R_COMPILE^OPT_COARRAY_DNODES<1>) -xopts=:MP_PROCS=1
