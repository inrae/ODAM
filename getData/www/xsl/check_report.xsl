<?xml version='1.0'?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
      <xsl:output method="html" indent="yes" media-type="text/html"/>

      <xsl:template match="text()" name="split">
         <xsl:param name="pText" select="."/>
         <xsl:if test="string-length($pText)">
            <xsl:if test="not($pText=.)">
                <br />
            </xsl:if>
            <xsl:value-of select=
                "substring-before(concat($pText,'\n'),'\n')"/>
            <xsl:call-template name="split">
                <xsl:with-param name="pText" select=
                   "substring-after($pText, '\n')"/>
            </xsl:call-template>
         </xsl:if>
      </xsl:template>

      <xsl:template match="/WSCheck">
                  <xsl:apply-templates select="Rows" mode="tblrows" />
                  <xsl:apply-templates select="Notes" mode="tblnotes" />
      </xsl:template>

      <xsl:template match="*" mode="tblrows">
           <xsl:variable name="count" select="@count" />
           <xsl:if test="$count > 0">
               <h1>Checklist</h1>
               <table style="border: 0px solid #B0BDCC; border-spacing: 2px; min-width: 50%">
                  <xsl:apply-templates select="./Row[1]" mode="header" />
                  <xsl:apply-templates select="./*" mode="row" />
               </table>
               <br /><br />
           </xsl:if>
      </xsl:template>

      <xsl:template match="*" mode="header">
            <tr style="background-color: #4a86e8; color: white">
                <xsl:apply-templates select="./*" mode="column" />
                <th>Status</th>
            </tr>
      </xsl:template>

      <xsl:template match="*" mode="row">
           <xsl:variable name="status" select="@status" />
            <xsl:element name="tr">
                <xsl:if test="position() mod 2 != 1">
                   <xsl:attribute  name="style">background-color:#EFF4F9</xsl:attribute>
                </xsl:if>
                <xsl:apply-templates select="./*" mode="node" />
                <xsl:element name="td">
                    <xsl:if test="normalize-space($status)='Error'">
                        <xsl:attribute  name="style">background-color:#EF0000</xsl:attribute>
                    </xsl:if>
                    <font size="+1"><strong><xsl:value-of select="$status" /> </strong></font>
                </xsl:element>
            </xsl:element>
      </xsl:template>

      <xsl:template match="*" mode="tblnotes">
           <xsl:variable name="count" select="@count" />
           <xsl:if test="$count > 0">
               <h1>Notes </h1>
               <table style="border: 0px solid #B0BDCC; border-spacing: 2px; min-width: 50%">
                  <tr style="background-color: #4a86e8; color: white">
                      <th>Ref. Note</th><th>Note Description</th>
                  </tr>
                  <xsl:apply-templates select="./*" mode="note" />
               </table>
           </xsl:if>
      </xsl:template>

      <xsl:template match="*" mode="note">
            <xsl:variable name="id" select="@id" />
            <xsl:element name="tr">
                <xsl:if test="position() mod 2 != 1">
                   <xsl:attribute  name="style">background-color:#EFF4F9</xsl:attribute>
                </xsl:if>
                <xsl:element name="td">
                    <font size="+1"><center><strong><xsl:value-of select="$id" /> </strong></center></font>
                </xsl:element>
                <xsl:apply-templates select="." mode="node" />
            </xsl:element>
      </xsl:template>

      <xsl:template match="*" mode="column">
            <xsl:element name="th">
                <xsl:value-of select="name(.)" /> 
            </xsl:element>
      </xsl:template>

      <xsl:template match="*" mode="node">
            <xsl:element name="td">
                <xsl:call-template name="split">
                </xsl:call-template >
            </xsl:element>
      </xsl:template>

</xsl:stylesheet>
