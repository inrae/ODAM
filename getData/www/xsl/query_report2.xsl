<?xml version='1.0'?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
      <xsl:output method="html" indent="yes" media-type="text/html"/>

      <xsl:template match="Array">
      <html>
      <head>
      <style type="text/css">
          table { padding: 0; font-family: Tahoma; font-size: 8pt;}
      </style>
      </head>
      <body>
      <table style="border: 0px solid #B0BDCC; border-spacing: 3px;">
                  <xsl:apply-templates select="Rows/Row[1]" mode="header" />
                  <xsl:apply-templates select="Rows/*" mode="row" />
      </table>
      </body>
      </html>
      </xsl:template>

      <xsl:template match="*" mode="header">
            <tr style="background-color: #4a86e8; color: white">
            <xsl:for-each select="*[substring(local-name(),1,2) !='CV']">
                <xsl:element name="th">
                   <xsl:value-of select="name(.)" /> 
                 </xsl:element>
            </xsl:for-each>
            <xsl:apply-templates select="CV_Term_ID|CV_term_id" mode="header2"/>
            </tr>
      </xsl:template>

      <xsl:template match="CV_Term_ID|CV_term_id" mode="header2">
            <th>CV_Term</th>
      </xsl:template>

      <xsl:template match="*" mode="row">
            <xsl:element name="tr">
                <xsl:if test="position() mod 2 != 1">
                   <xsl:attribute  name="style">background-color:#EFF4F9</xsl:attribute>
                </xsl:if>
                <xsl:apply-templates select="*[substring(local-name(),1,2) !='CV']" mode="node" />
                <xsl:apply-templates select="CV_Term_ID|CV_term_id" mode="row2"/>
            </xsl:element>
      </xsl:template>

      <xsl:template match="CV_Term_ID|CV_term_id" mode="row2">
            <xsl:element name="td">
            <xsl:element name="a">
                <xsl:attribute  name="href"><xsl:value-of select="." /></xsl:attribute>
                <xsl:attribute  name="target">_blank</xsl:attribute>
                <xsl:value-of select="following-sibling::node()" />
            </xsl:element>
            </xsl:element>
      </xsl:template>

      <xsl:template match="*" mode="node">
            <xsl:element name="td">
                <xsl:value-of select="." />
            </xsl:element>
      </xsl:template>

</xsl:stylesheet>
