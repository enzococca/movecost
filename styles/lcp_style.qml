<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis version="3.42" styleCategories="AllStyleCategories" simplifyAlgorithm="0" simplifyDrawingHints="1" simplifyLocal="1" simplifyDrawingTol="1" simplifyMaxScale="1" hasScaleBasedVisibilityFlag="0" maxScale="0" minScale="100000000" labelsEnabled="1" symbologyReferenceScale="-1">
  <renderer-v2 type="graduatedSymbol" attr="cost" graduatedMethod="GraduatedColor" symbollevels="0" enableorderby="0" forceraster="0">
    <ranges>
      <range lower="0" upper="1" symbol="0" label="0 - 1 h" render="true"/>
      <range lower="1" upper="2" symbol="1" label="1 - 2 h" render="true"/>
      <range lower="2" upper="4" symbol="2" label="2 - 4 h" render="true"/>
      <range lower="4" upper="8" symbol="3" label="4 - 8 h" render="true"/>
      <range lower="8" upper="100" symbol="4" label="> 8 h" render="true"/>
    </ranges>
    <symbols>
      <symbol type="line" name="0" alpha="1" clip_to_extent="1" force_rhr="0">
        <data_defined_properties>
          <Option type="Map">
            <Option type="QString" name="name" value=""/>
            <Option name="properties"/>
            <Option type="QString" name="type" value="collection"/>
          </Option>
        </data_defined_properties>
        <layer locked="0" enabled="1" pass="0" class="SimpleLine">
          <Option type="Map">
            <Option type="QString" name="line_color" value="26,150,65,255"/>
            <Option type="QString" name="line_style" value="solid"/>
            <Option type="QString" name="line_width" value="1.2"/>
            <Option type="QString" name="line_width_unit" value="MM"/>
          </Option>
        </layer>
        <layer locked="0" enabled="1" pass="0" class="MarkerLine">
          <Option type="Map">
            <Option type="QString" name="interval" value="15"/>
            <Option type="QString" name="interval_unit" value="MM"/>
            <Option type="QString" name="placement" value="interval"/>
          </Option>
          <data_defined_properties>
            <Option type="Map">
              <Option type="QString" name="name" value=""/>
              <Option name="properties"/>
              <Option type="QString" name="type" value="collection"/>
            </Option>
          </data_defined_properties>
          <symbol type="marker" name="@0@1" alpha="1" clip_to_extent="1" force_rhr="0">
            <layer locked="0" enabled="1" pass="0" class="SimpleMarker">
              <Option type="Map">
                <Option type="QString" name="color" value="26,150,65,255"/>
                <Option type="QString" name="name" value="filled_arrowhead"/>
                <Option type="QString" name="size" value="3"/>
                <Option type="QString" name="size_unit" value="MM"/>
              </Option>
            </layer>
          </symbol>
        </layer>
      </symbol>
      <symbol type="line" name="1" alpha="1" clip_to_extent="1" force_rhr="0">
        <layer locked="0" enabled="1" pass="0" class="SimpleLine">
          <Option type="Map">
            <Option type="QString" name="line_color" value="166,217,106,255"/>
            <Option type="QString" name="line_style" value="solid"/>
            <Option type="QString" name="line_width" value="1.2"/>
            <Option type="QString" name="line_width_unit" value="MM"/>
          </Option>
        </layer>
        <layer locked="0" enabled="1" pass="0" class="MarkerLine">
          <Option type="Map">
            <Option type="QString" name="interval" value="15"/>
            <Option type="QString" name="interval_unit" value="MM"/>
            <Option type="QString" name="placement" value="interval"/>
          </Option>
          <symbol type="marker" name="@1@1" alpha="1" clip_to_extent="1" force_rhr="0">
            <layer locked="0" enabled="1" pass="0" class="SimpleMarker">
              <Option type="Map">
                <Option type="QString" name="color" value="166,217,106,255"/>
                <Option type="QString" name="name" value="filled_arrowhead"/>
                <Option type="QString" name="size" value="3"/>
                <Option type="QString" name="size_unit" value="MM"/>
              </Option>
            </layer>
          </symbol>
        </layer>
      </symbol>
      <symbol type="line" name="2" alpha="1" clip_to_extent="1" force_rhr="0">
        <layer locked="0" enabled="1" pass="0" class="SimpleLine">
          <Option type="Map">
            <Option type="QString" name="line_color" value="255,255,0,255"/>
            <Option type="QString" name="line_style" value="solid"/>
            <Option type="QString" name="line_width" value="1.2"/>
            <Option type="QString" name="line_width_unit" value="MM"/>
          </Option>
        </layer>
        <layer locked="0" enabled="1" pass="0" class="MarkerLine">
          <Option type="Map">
            <Option type="QString" name="interval" value="15"/>
            <Option type="QString" name="interval_unit" value="MM"/>
            <Option type="QString" name="placement" value="interval"/>
          </Option>
          <symbol type="marker" name="@2@1" alpha="1" clip_to_extent="1" force_rhr="0">
            <layer locked="0" enabled="1" pass="0" class="SimpleMarker">
              <Option type="Map">
                <Option type="QString" name="color" value="255,255,0,255"/>
                <Option type="QString" name="name" value="filled_arrowhead"/>
                <Option type="QString" name="size" value="3"/>
                <Option type="QString" name="size_unit" value="MM"/>
              </Option>
            </layer>
          </symbol>
        </layer>
      </symbol>
      <symbol type="line" name="3" alpha="1" clip_to_extent="1" force_rhr="0">
        <layer locked="0" enabled="1" pass="0" class="SimpleLine">
          <Option type="Map">
            <Option type="QString" name="line_color" value="253,174,97,255"/>
            <Option type="QString" name="line_style" value="solid"/>
            <Option type="QString" name="line_width" value="1.2"/>
            <Option type="QString" name="line_width_unit" value="MM"/>
          </Option>
        </layer>
        <layer locked="0" enabled="1" pass="0" class="MarkerLine">
          <Option type="Map">
            <Option type="QString" name="interval" value="15"/>
            <Option type="QString" name="interval_unit" value="MM"/>
            <Option type="QString" name="placement" value="interval"/>
          </Option>
          <symbol type="marker" name="@3@1" alpha="1" clip_to_extent="1" force_rhr="0">
            <layer locked="0" enabled="1" pass="0" class="SimpleMarker">
              <Option type="Map">
                <Option type="QString" name="color" value="253,174,97,255"/>
                <Option type="QString" name="name" value="filled_arrowhead"/>
                <Option type="QString" name="size" value="3"/>
                <Option type="QString" name="size_unit" value="MM"/>
              </Option>
            </layer>
          </symbol>
        </layer>
      </symbol>
      <symbol type="line" name="4" alpha="1" clip_to_extent="1" force_rhr="0">
        <layer locked="0" enabled="1" pass="0" class="SimpleLine">
          <Option type="Map">
            <Option type="QString" name="line_color" value="215,25,28,255"/>
            <Option type="QString" name="line_style" value="solid"/>
            <Option type="QString" name="line_width" value="1.2"/>
            <Option type="QString" name="line_width_unit" value="MM"/>
          </Option>
        </layer>
        <layer locked="0" enabled="1" pass="0" class="MarkerLine">
          <Option type="Map">
            <Option type="QString" name="interval" value="15"/>
            <Option type="QString" name="interval_unit" value="MM"/>
            <Option type="QString" name="placement" value="interval"/>
          </Option>
          <symbol type="marker" name="@4@1" alpha="1" clip_to_extent="1" force_rhr="0">
            <layer locked="0" enabled="1" pass="0" class="SimpleMarker">
              <Option type="Map">
                <Option type="QString" name="color" value="215,25,28,255"/>
                <Option type="QString" name="name" value="filled_arrowhead"/>
                <Option type="QString" name="size" value="3"/>
                <Option type="QString" name="size_unit" value="MM"/>
              </Option>
            </layer>
          </symbol>
        </layer>
      </symbol>
    </symbols>
    <source-symbol>
      <symbol type="line" name="0" alpha="1" clip_to_extent="1" force_rhr="0">
        <layer locked="0" enabled="1" pass="0" class="SimpleLine">
          <Option type="Map">
            <Option type="QString" name="line_color" value="26,150,65,255"/>
            <Option type="QString" name="line_style" value="solid"/>
            <Option type="QString" name="line_width" value="1.2"/>
            <Option type="QString" name="line_width_unit" value="MM"/>
          </Option>
        </layer>
      </symbol>
    </source-symbol>
    <colorramp type="gradient" name="[source]">
      <Option type="Map">
        <Option type="QString" name="color1" value="26,150,65,255"/>
        <Option type="QString" name="color2" value="215,25,28,255"/>
        <Option type="QString" name="discrete" value="0"/>
        <Option type="QString" name="rampType" value="gradient"/>
        <Option type="QString" name="stops" value="0.25;166,217,106,255:0.5;255,255,0,255:0.75;253,174,97,255"/>
      </Option>
    </colorramp>
    <classificationMethod id="EqualInterval">
      <symmetricMode enabled="0" symmetrypoint="0" astride="0"/>
      <labelFormat format="%1 - %2" labelprecision="1" trimtrailingzeroes="1"/>
      <extraInformation/>
    </classificationMethod>
  </renderer-v2>
  <labeling type="simple">
    <settings calloutType="simple">
      <text-style fontSize="8" fontFamily="Sans Serif" textColor="50,50,50,255" fieldName="round(cost, 2) || ' h'">
        <text-buffer bufferSize="0.8" bufferColor="255,255,255,255" bufferDraw="1"/>
      </text-style>
      <placement placement="2" repeatDistance="100" repeatDistanceUnits="MM"/>
    </settings>
  </labeling>
  <blendMode>0</blendMode>
</qgis>
