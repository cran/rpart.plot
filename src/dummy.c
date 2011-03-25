/* dummy.c:  This file exists to pacify rcmd check.

This file is needed because we need somewhere to store the tests
directory (other than at the root of this package, because we do not
want to run the tests automatically).  The tests directory is big and we
don't want people to download it unnecessarily when they install the
package. It seems that the only way we can achieve that is to store the
tests in the src directory.  But an empty src directory causes rcmd check
to fail.  Hence this dummy file.

*/

void dummy(void)
{
}
