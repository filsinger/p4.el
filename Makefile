#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#    If you have any problems to report, or suggestions, please send them
#    to p4el-bugs@lists.sourceforge.net
#
#	$Id: Makefile,v 1.2 2002/10/24 16:27:52 rvgnu Exp $

WEB=	TODO index.php p4.el.html thanks.html
P4EL=	p4.el ChangeLog
REL=	10.2

WEBTGZ=	web.tgz
P4TGZ=	p4.el-$(REL).tgz

all: $(WEBTGZ) $(P4TGZ)

$(P4TGZ): $(P4EL)
	@echo Creating p4.el distribution $@
	@tar zcfp $@ $^

$(WEBTGZ): $(WEB)
	@echo Creating Web distribution $@
	@tar zcfp $@ $^

clean:
	@rm $(P4TGZ) $(WEBTGZ)
