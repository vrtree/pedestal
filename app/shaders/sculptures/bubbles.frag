#version 330 core


const float MAX_TRACE_DISTANCE = 2.;           // max trace distance
const float INTERSECTION_PRECISION = 0.005;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 20;
const float PI  = 3.14159;

uniform float uTime;
uniform vec3 uDimensions;

in vec3 vPos;
in vec3 vEye;
in vec3 vNorm;

in vec3 vHand1;
in vec3 vHand2;

in vec2 vUV;

out vec4 color;

vec3 sPos[10];


float sdSphere( vec3 p, float s ){
  return length(p)-s;
}

vec2 smoothU( vec2 d1, vec2 d2, float k)
{
    float a = d1.x;
    float b = d2.x;
    float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
    return vec2( mix(b, a, h) - k*h*(1.0-h), mix(d2.y, d1.y, pow(h, 2.0)));
}

float sdBox( vec3 p, vec3 b )
{
  vec3 d = abs(p) - b;
  return min(max(d.x,max(d.y,d.z)),0.0) +
         length(max(d,0.0));
}

float opU( float d1, float d2 ){
  return min(d1,d2);
}

vec2 opU( vec2 d1, vec2 d2 )
{
    return  d1.x < d2.x ? d1 : d2 ;
}

float opS( float d1, float d2 )
{
    return max(-d1,d2);
}

//--------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 pos ){

  //pos *= 2.;

  vec2 res = vec2( sdBox( pos  , vec3(1.) * uDimensions ) , 1. );
  res.x = opS( sdBox( pos  , .5 + vec3( INTERSECTION_PRECISION * 1.1)) , res.x );


  float s;

  //for( int i = 0; i < 6; i++){
  s = sdSphere( pos - sPos[0], .03  );
  res = smoothU( res , vec2( s , 2.) , .1);
  s = sdSphere( pos - sPos[1], .03  );
  res = smoothU( res , vec2( s , 2.) , .1);
  s = sdSphere( pos - sPos[2], .03  );
  res = smoothU( res , vec2( s , 2.) , .1);
  s = sdSphere( pos - sPos[3], .03  );
  res = smoothU( res , vec2( s , 2.) , .1);
  s = sdSphere( pos - sPos[4], .03  );
  res = smoothU( res , vec2( s , 2.) , .1);
  s = sdSphere( pos - sPos[5], .03  );
  res = smoothU( res , vec2( s , 2.) , .1 );
  //}

  //vec2 res = vec2( sdBox( pos + vec3( 0., .5, 0.) , vec3( 2. , .1 , 2. ) ) , 1. );

 // res.x = opS(  res.x , sdBox( pos , vec3( .125 )) ) ;

  return res;

}


vec2 calcIntersection( in vec3 ro, in vec3 rd ){

    float h =  INTERSECTION_PRECISION * 2.0;
    float t = 0.0;
    float res = -1.0;
    float id = -1.;
    
    for( int i=0; i < NUM_OF_TRACE_STEPS ; i++ ){
        
        if( h < INTERSECTION_PRECISION || t > MAX_TRACE_DISTANCE * 2.) break;
        vec2 m = map( ro+rd*t );
        h = m.x;
        t += h;
        id = m.y;
        
    }

    if( t < MAX_TRACE_DISTANCE ) res = t;
    if( t > MAX_TRACE_DISTANCE ) id =-1.0;
    
    return vec2( res , id );
     
}

// Calculates the normal by taking a very small distance,
// remapping the function, and getting normal for that
vec3 calcNormal( in vec3 pos ){
    
  vec3 eps = vec3( 0.001, 0.0, 0.0 );
  vec3 nor = vec3(
      map(pos+eps.xyy).x - map(pos-eps.xyy).x,
      map(pos+eps.yxy).x - map(pos-eps.yxy).x,
      map(pos+eps.yyx).x - map(pos-eps.yyx).x );

  return normalize(nor);
}


void main(){

  sPos[0] = vec3( sin( uTime * .14 ) * .1 , sin( uTime  * .25 ) * .2 -.1,  sin( uTime  * .25 ) * .1) * uDimensions.x * 2.;
  sPos[1] = vec3( sin( uTime * .34 ) * .1 , sin( uTime  * .825 ) * .2 -.1,  sin( uTime  * .25 ) * .1)* uDimensions.x * 2.;
  sPos[2] = vec3( sin( uTime * .24 ) * .1 , sin( uTime  * .4125 ) * .2 -.1,  sin( uTime  * .25 ) * .1)* uDimensions.x * 2.;
  sPos[3] = vec3( sin( uTime * .19 + 1. ) * .1 , sin( uTime  * .9625 ) * .2 -.1,  sin( uTime  * .25 ) * .1)* uDimensions.x * 2.;
  sPos[4] = vec3( sin( uTime * .54 + 2. ) * .1 , sin( uTime  * .31225 ) * .2 -.1,  sin( uTime  * .25 ) * .1)* uDimensions.x * 2.;
  sPos[5] = vec3( sin( uTime * .4 + .3 ) * .1 , sin( uTime  * .625 ) * .2 -.1,  sin( uTime  * .25 ) * .1)* uDimensions.x * 2.;

  vec3 ro = vPos;
  vec3 rd = normalize( vPos - vEye );


  vec3 handDir1 = normalize( vHand1 - ro);
  vec3 handDir2 = normalize( vHand2 - ro);

  vec2 res = calcIntersection( ro , rd );


  vec3 col = vec3( 0. );
  if( res.y > .5 ){

    vec3 pos = ro + rd * res.x;

    vec3 handDir1 = normalize( vHand1 - pos);
    vec3 handDir2 = normalize( vHand2 - pos);
    vec3 norm;

    
    norm = calcNormal( pos );

    col = norm * .5 + .5;



  }


  if( abs(vUV.x - .5) > .49 ||  abs(vUV.y - .5) > .49 ){
    col = vec3(1.);
  }


  color = vec4( col , 1. );



}