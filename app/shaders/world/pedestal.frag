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

    vec3 nNorm = -vNormal;

    vec3 refl = reflect( lightDir , nNorm );
    vec3 eyeDir = normalize( uEye - vPosition );

    float spec = max( 0. , dot( eyeDir, refl ));


    float lPower = 5. / pow( length( light ), 5.);
    float lDot = dot( nNorm , lightDir );

    vec3 col = vec3( lDot * lPower) + vec3( lPower ) + vec3( 1. * pow(spec, 100. ));

    if( abs(vUV.x - .5) > .49 ||  abs(vUV.y - .5) > .49 ){
      col += vec3(.4);
    }
    fragColor = vec4( col * .5, 1.);

}