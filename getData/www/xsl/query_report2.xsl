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

      <xsl:template match="Count">
            <xsl:element name="strong">
            <xsl:text># </xsl:text>
                <xsl:value-of select="." />
            <xsl:text> rows</xsl:text>
            </xsl:element>
      </xsl:template>

      <xsl:template match="*" mode="header">
            <tr style="background-color: #4a86e8; color: white">
                <xsl:apply-templates select="./*" mode="column" />
            </tr>
      </xsl:template>

      <xsl:template match="*" mode="row">
            <xsl:element name="tr">
                <xsl:if test="position() mod 2 != 1">
                   <xsl:attribute  name="style">background-color:#EFF4F9</xsl:attribute>
                </xsl:if>
                <xsl:apply-templates select="./*" mode="node" />
            </xsl:element>
      </xsl:template>

      <xsl:template match="*" mode="column">
            <xsl:element name="th">
                <xsl:value-of select="name(.)" /> 
            </xsl:element>
      </xsl:template>

      <xsl:template match="*" mode="node">
            <xsl:element name="td">
                <xsl:value-of select="." />
            </xsl:element>
      </xsl:template>

</xsl:stylesheet>
