// package com.github.codingdebugallday.driver.common.infra.session.factory;
//
// import java.util.Map;
// import java.util.Optional;
// import java.util.concurrent.ConcurrentHashMap;
//
// import com.github.codingdebugallday.driver.common.infra.exceptions.DriverSessionFactoryException;
// import com.github.codingdebugallday.driver.common.infra.session.DriverSession;
//
// /**
//  * <p>
//  * description
//  * </p>
//  *
//  * @author isaac 2020/7/6 17:05
//  * @since 1.0
//  */
// public class DriverSessionManager implements DriverSessionFetcher {
//
//     private final Map<String, DriverSessionFactory> driverSessionFactories = new ConcurrentHashMap<>();
//
//     @Override
//     public DriverSession getDriverSession(String datasourceCode) {
//         return Optional.ofNullable(driverSessionFactories.get(datasourceCode))
//                 .orElseThrow(() -> new DriverSessionFactoryException("session factory not exist"))
//                 .getDriverSession(datasourceCode);
//     }
//
//     /**
//      * 在删除/停止插件后，需要remove掉
//      *
//      * @param datasourceCode datasourceCode
//      */
//     public void remove(String datasourceCode) {
//         driverSessionFactories.remove(datasourceCode);
//     }
//
//     /**
//      * 新增DriverSessionFactory
//      *
//      * @param datasourceCode       datasourceCode
//      * @param driverSessionFactory DriverSessionFactory
//      */
//     public void put(String datasourceCode, DriverSessionFactory driverSessionFactory) {
//         driverSessionFactories.put(datasourceCode, driverSessionFactory);
//     }
//
// }
