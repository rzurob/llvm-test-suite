#===------------------------------------------------------------------------===#
#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
#===------------------------------------------------------------------------===#

# There are currently no unsupported files.
set(UNSUPPORTED_FILES "")

# There are currently no unimplemented files.
set(UNIMPLEMENTED_FILES "")

# These tests are disabled because they cause internal compiler errors.
file(GLOB SKIPPED_FILES CONFIGURE_DEPENDS
  ""
)

# These tests are disabled because they fail at runtime when they should pass.
file(GLOB FAILING_FILES CONFIGURE_DEPENDS
  ""
)
