<?php
//    This program is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, write to the Free Software
//    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//
//    If you have any problems to report, or suggestions, please send them
//    to p4el-bugs@lists.sourceforge.net
//
//	$Id: index.php,v 1.8 2002/10/24 16:27:52 rvgnu Exp $

function fstats($file, &$kbsize)
{
    $stats = stat($file);
    $kbsize = (int)($stats[7] / 1024);
}
?>

<html>
  <head>
    <title>Emacs P4 Integration Home Page</title>
    <META name="description"
	  content="P4 Emacs Lisp Integration - www.dsmit.com.">
    <META name="keywords"
	  content="emacs, lisp, elisp, emacs-lisp, www.dsmit.com, dsmit, p4, whitespace, perforce, windows explorer">
  </head>

<center>
<img src="title.gif" alt="Emacs-P4 Integration Page">
</center>

<BODY BACKGROUND="../gifs/bckgtile.gif">
<A NAME="top">

<p>We would like to thank <a
href="http://sourceforge.net/">Sourceforge.net</a> for graciously
providing the Emacs-P4 library a home on the Internet.
<p>
<a href="http://www.perforce.com">Perforce</a> is an SCM System that
	integrates very well with <a
	href="http://www.gnu.org/software/emacs">Emacs</a> and <a
	href="http://www.xemacs.org">XEmacs</a>.
<p>
<b>p4.el</b> is an Emacs-Lisp Library that can be used with Emacs or XEmacs.
<p>
We would like to <a href="thanks.html">thank</a> the many people who have
been involved in creating and improving <b>p4.el</b>. Without their help,
this package would not be what it is today.
<p>
<B>The files are now released under the <a href="http://sourceforge.net/project/showfiles.php?group_id=58201">Sourceforge Release System</a></B>
<p>
A helpful guide to p4.el is available <a href="p4.el.html">here</a>, thanks
to our users.
<p>
<b>Other Interesting Links</b>
<ul>
<?php
$p4_explorer_ver = "1.1";
$p4_explorer = "p4-explorer-" . $p4_explorer_ver . ".tar.gz";
fstats($p4_explorer, $p4explorer_size);

$p4_explorer_ss = "p4-explorer-screen-shots.zip";
fstats($p4_explorer_ss, $p4explorer_ss_size);
?>

<li><a href="<?php echo $p4_explorer; ?>">P4-Windows
Explorer Integration package (<?php echo $p4explorer_size; ?>KB)</a>
is also available (ins a rudimentary form) for those who would like to
perform P4 operations by right-clicking on a file or a directory. The
current version is <b><?php echo $p4_explorer_ver; ?></b>. The format
of the distribution is <b>a gzipped-tar file (.tar.gz)</b>.
<br>
<a href="<?php echo $p4_explorer_ss; ?>">P4-Windows
Explorer Screen shots (<?php echo $p4explorer_ss_size; ?>KB)</a>
</li>

</ul>
<p>
<hr><i>Maintainer: <a href="mailto:rv@no.spam.dsmit.com">Rajesh Vaidheeswarran (remove the no.spam from my address to email me)</a></i>
<p>
<i><a href="http://sourceforge.net/projects/p4el/">P4.el project page on Sourceforge.net</a></i>
<p>
<A href="http://sourceforge.net"> <IMG src="http://sourceforge.net/sflogo.php?group_id=58201&amp;type=5" width="210" height="62" border="0" alt="SourceForge Logo"></A>
<hr>
Last Updated: $Revision: 1.8 $ on $Date: 2002/10/24 16:27:52 $
</body>
</html>
