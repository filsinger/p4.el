<?php

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
<center>
<table width=50%>
<th width=50%><img src="released.gif" alt="Released Versions"></th>
<th width=50%><img src="beta.gif" alt="Beta Versions"></th>
<tr>
<td>
<?php
    $released = file("RELEASED");
    $released[0] = chop($released[0]);
    $released[1] = chop($released[1]);
    $beta = file("BETA");
    $beta[0] = chop($beta[0]);
    $beta[1] = chop($beta[1]);
?>

<!-- Release Versions Table -->
<table border=1 align=center>
<th width=50% align=center><font size=-1>Status</font></th>
<th width=50% align=center><font size=-1>Version</font></th>

<tr align=center>
<?php
$released_chlog_0 = "ChangeLog-$released[0]";
fstats($released_chlog_0, $released_chlog_0_size);
$released_chlog_1 = "ChangeLog-$released[1]";
fstats($released_chlog_1, $released_chlog_1_size);

$released_p4el_0 = "p4.el-$released[0]";
fstats($released_p4el_0, $released_p4el_0_size);
$released_p4el_1 = "p4.el-$released[1]";
fstats($released_p4el_1, $released_p4el_1_size);
?>

<td><a href="<?php echo $released_chlog_0; ?>">Current (<?php echo $released_chlog_0_size; ?>KB)</a></td>
<td><a href="<?php echo $released_p4el_0; ?>"><?php echo $released[0]; ?>(<?php echo $released_p4el_0_size; ?>KB)</a><td>
</tr>
<tr align=center>
<td><a href="<?php echo $released_chlog_1; ?>">Previous (<?php echo $released_chlog_1_size; ?>KB)</a></td>
<td><a href="<?php echo $released_p4el_1; ?>"><?php echo $released[1]; ?>(<?php echo $released_p4el_1_size; ?>KB)</a><td>
</tr>
</table>
</td>

<td>
<!-- Beta Versions Table -->
<table border=1 align=center>
<th width=50% align=center><font size=-1>Status</font></th>
<th width=50% align=center><font size=-1>Version</font></th>

<?php
$beta_chlog_0 = "ChangeLog-$beta[0]";
fstats($beta_chlog_0, $beta_chlog_0_size);
$beta_chlog_1 = "ChangeLog-$beta[1]";
fstats($beta_chlog_1, $beta_chlog_1_size);

$beta_p4el_0 = "p4.el-$beta[0]";
fstats($beta_p4el_0, $beta_p4el_0_size);
$beta_p4el_1 = "p4.el-$beta[1]";
fstats($beta_p4el_1, $beta_p4el_1_size);
?>

<tr align=center>
<td><a href="<?php echo $beta_chlog_0; ?>">Current (<?php echo $beta_chlog_0_size; ?>KB)</a></td>
<td><a href="<?php echo $beta_p4el_0; ?>"><?php echo $beta[0]; ?>(<?php echo $beta_p4el_0_size; ?>KB)</a><td>
</tr>
<tr align=center>
<td><a href="<?php echo $beta_chlog_1; ?>">Previous (<?php echo $beta_chlog_1_size; ?>KB)</a></td>
<td><a href="<?php echo $beta_p4el_1; ?>"><?php echo $beta[1]; ?>(<?php echo $beta_p4el_1_size; ?>KB)</a><td>
</tr>
</table>
</td>

</tr>
</table>
</center>
<p>
<!--
<i>Please Note that 7.0 and 7.1 <b>will not</b> work with <b>GNU Emacs
19.34</b> since a lot of `customize' related changes have been done to be
compliant with 20.x and above. If you are using GNU Emacs 19.34, then your
release is <i>7.2 or above</i></a></i>
-->
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
<hr><i>Maintainer: <a href="mailto:rv@dsmit.com">Rajesh Vaidheeswarran</a></i>
<p>
<i><a href="http://sourceforge.net/projects/p4el/">P4.el project page Sourceforge.net</a></i>
</body>
</html>
