#version 330 core

uniform vec3 uEye;
uniform vec3 uLight;

in      vec3 vPosition;
in      vec3 vNormal;
in      vec2 vUV;

out     vec4 fragColor;


void main() {

    vec3 light = vPosition - uLight;
    vec3 lightDir = normalize( light );

  /*  vec3 nNorm = calcNormal( vPosition );

    nNorm *= .1;
    nNorm += vNormal;
    nNorm = normalize( nNorm );*/

    vec3 nNorm = vNormal;


    float lPower = .5 / length( light );
    float lDot = dot( nNorm , lightDir );

    vec3 col = vec3( lDot * lPower);

    if( abs(vUV.x - .5) > .49 ||  abs(vUV.y - .5) > .49 ){
      col *= 2.;
    }
    fragColor = vec4( col , 1.);

}