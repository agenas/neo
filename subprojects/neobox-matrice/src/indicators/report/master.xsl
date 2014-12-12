<!--
	~ Copyright 2014 Stefano Gualdi, Fabrizio Carinci, AGENAS.
	~
	~ Licensed under the European Union Public Licence (EUPL), Version 1.1 (the "License");
	~ you may not use this file except in compliance with the License.
	~ You may obtain a copy of the License at
	~
	~       http://joinup.ec.europa.eu/software/page/eupl/licence-eupl
	~
	~ Unless required by applicable law or agreed to in writing, software
	~ distributed under the License is distributed on an "AS IS" BASIS,
	~ WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
	~ See the License for the specific language governing permissions and
	~ limitations under the License.
	-->

<xsl:stylesheet version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:fo="http://www.w3.org/1999/XSL/Format">

	<xsl:import href="res:xsl/docbook/fo/docbook.xsl"/>
 <xsl:param name="fop1.extensions" select="1"/>

	<!-- Basic page setup -->
	<xsl:param name="paper.type" select="'A4'"/>
	<xsl:param name="page.margin.top">1.0cm</xsl:param>
	<xsl:param name="page.margin.bottom">1.0cm</xsl:param>
	<xsl:param name="page.margin.inner">1.5cm</xsl:param>
	<xsl:param name="page.margin.outer">1.5cm</xsl:param>
	<xsl:param name="body.start.indent">0pt</xsl:param>
	<xsl:param name="body.end.indent">0pt</xsl:param>

	<!-- Clear verso -->
	<xsl:template name="book.titlepage.verso"/>

	<!-- Clear page break after verso -->
	<xsl:template name="book.titlepage.before.verso"/>

	<!-- No word wrap (if possible) in page headers -->
	<xsl:param name="header.column.widths">0 1 0</xsl:param>

	<!-- Customize chapters and tables titles -->
	<xsl:param name="local.l10n.xml" select="document('')"/>
	<l:i18n xmlns:l="http://docbook.sourceforge.net/xmlns/l10n/1.0">
		<l:l10n language="en">
			<l:context name="title-numbered">
				<l:template name="chapter" text="%t"/>
			</l:context>
	    	<l:context name="title">
	      		<l:template name="table" text="%t"/>
	    	</l:context>
		</l:l10n>
		<l:l10n language="it">
			<l:context name="title-numbered">
				<l:template name="chapter" text="%t"/>
			</l:context>
	    	<l:context name="title">
	      		<l:template name="table" text="%t"/>
	    	</l:context>
		</l:l10n>
	</l:i18n>
	<xsl:param name="chapter.autolabel">0</xsl:param>

	<!-- Font size for Chapter Title -->
	<xsl:attribute-set name="component.title.properties">
		<xsl:attribute name="font-size">16pt</xsl:attribute>
	</xsl:attribute-set>

	<!-- Font size for Section Title -->
 <xsl:attribute-set name="section.title.level1.properties">
  <xsl:attribute name="font-weight">bold</xsl:attribute>
  <xsl:attribute name="font-size">16pt</xsl:attribute>
 </xsl:attribute-set>

	<!-- Font size for internal tiles (eg. tables' titles) -->
	<xsl:attribute-set name="formal.title.properties" use-attribute-sets="normal.para.spacing">
		<xsl:attribute name="font-weight">bold</xsl:attribute>
		<xsl:attribute name="font-size">12pt</xsl:attribute>
	</xsl:attribute-set>

 <xsl:template match="*[@role='red']" priority="10">
  <fo:inline color="red" font-size="12" font-weight="bold"><xsl:apply-imports/></fo:inline>
 </xsl:template>

 <xsl:template match="*[@role='small']" priority="10">
  <fo:inline font-size="10" font-weight="bold" line-height="3pt" line-stacking-strategy="font-height">
   <xsl:apply-imports/>
  </fo:inline>
 </xsl:template>

 <xsl:template match="*[@role='verysmall']" priority="10">
  <fo:inline font-size="8" font-weight="bold" line-height="3pt" line-stacking-strategy="font-height">
   <xsl:apply-imports/>
  </fo:inline>
 </xsl:template>

 <xsl:param name="toc.section.depth">0</xsl:param>

	<!-- Customize TOCs -->
	<xsl:param name="generate.toc">
	    appendix  toc,title
	    article/appendix  nop
	    article   toc,title
	    book      toc,title
	    chapter   toc,title
	    part      toc,title
	    preface   toc,title
	    qandadiv  toc
	    qandaset  toc
	    reference toc,title
	    sect1     toc
	    sect2     toc
	    sect3     toc
	    sect4     toc
	    sect5     toc
	    section   toc
	    set       toc,title
	</xsl:param>

	<xsl:attribute-set name="toc.line.properties">
		<xsl:attribute name="font-size">14pt</xsl:attribute>
 	</xsl:attribute-set>

 <xsl:attribute-set name="toc.margin.properties">
  <xsl:attribute name="break-after">page</xsl:attribute>
 </xsl:attribute-set>

 <!-- Customize tables -->
	<xsl:attribute-set name="table.properties" use-attribute-sets="formal.object.properties">
		<xsl:attribute name="keep-together.within-column">auto</xsl:attribute>
	</xsl:attribute-set>
	<xsl:attribute-set name="table.cell.padding">
		<xsl:attribute name="padding-left">1pt</xsl:attribute>
		<xsl:attribute name="padding-right">1pt</xsl:attribute>
		<xsl:attribute name="padding-top">0.2pt</xsl:attribute>
		<xsl:attribute name="padding-bottom">0.2pt</xsl:attribute>
	</xsl:attribute-set>
	<xsl:param name="keep.row.together">1</xsl:param>

	<!-- Page break -->
	<xsl:template match="processing-instruction('custom-pagebreak')">
		<fo:block break-before='page'/>
	</xsl:template>

	<!-- Line break -->
	<xsl:template match="processing-instruction('custom-linebreak')">
		<fo:block />
	</xsl:template>
</xsl:stylesheet>
